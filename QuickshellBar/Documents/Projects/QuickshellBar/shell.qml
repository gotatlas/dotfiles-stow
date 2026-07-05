import Quickshell
import Quickshell.Io
import QtQuick
import Quickshell.Services.SystemTray
import Quickshell.Widgets
import Quickshell.Services.Notifications

ShellRoot {
    id: root

    property color bg: "#282a36"
    property color fg: "#f8f8f2"
    property color sel: "#6272a4"
    property color dim: "#44475a"
    property color red: "#ff5555"

    property string fontName: "Spleen"
    property int barHeight: 13
    property int fontSize: 12
    property string statsText: ""
    property string privacyText: ""

    property string notificationText: ""
    property int notificationCount: 0

    /* NotificationServer { */
    /*     id: notificationServer */

    /*     // Keep it simple first. */
    /*     bodySupported: true */
    /*     bodyMarkupSupported: false */
    /*     imageSupported: false */
    /*     actionsSupported: false */
    /*     persistenceSupported: true */

    /*     onNotification: function(notification) { */
    /*         notification.tracked = true; */

    /*         root.notificationCount = notificationServer.trackedNotifications.count; */

    /*         var app = notification.appName || ""; */
    /*         var summary = notification.summary || ""; */
    /*         var body = notification.body || ""; */

    /*         if (app.length > 0 && summary.length > 0) { */
    /*             root.notificationText = app + ": " + summary; */
    /*         } else if (summary.length > 0) { */
    /*             root.notificationText = summary; */
    /*         } else if (body.length > 0) { */
    /*             root.notificationText = body; */
    /*         } else { */
    /*             root.notificationText = "notification"; */
    /*         } */
    /*     } */
    /* } */

    QtObject {
        id: niri

        property var workspaces: []
        property var windowsById: ({})
        property var visibleWorkspaces: []
        property string focusedWindowTitle: ""
        
        function clone(obj) {
            var out = {};
            for (var key in obj) {
                out[key] = obj[key];
            }
            return out;
        }

        function labelFor(ws) {
            if (ws.name && ws.name.length > 0) {
                return ws.name;
            }

            return "+";
        }

        function workspaceHasWindows(ws) {
            for (var id in windowsById) {
                var win = windowsById[id];
                
                if (win && win.workspace_id === ws.id) {
                    return true;
                }
            }

            return false;
        }

        function updateFocusedWindowTitle() {
            var title = "";

            /* for (var id in windowsById) { */
            /*     var win = windowsById[id]; */

            /*     if (win && win.is_focused) { */
            /*         if (win.title && win.title.length > 0) { */
            /*             title = win.title; */
            /*         } else if (win.app_id && win.app_id.length > 0) { */
            /*             title = win.app_id; */
            /*         } */

            /*         focusedWindowTitle = title; */
            /*         return; */
            /*     } */
            /* } */

            
            // Fallback (now main)
            var i = 0;

            while (i < workspaces.length) {
                var ws = workspaces[i];

                if (ws.is_focused) {
                    if (ws.active_window_id !== null && ws.active_window_id !== undefined) {
                        var fallback = windowsById[ws.active_window_id];

                        if (fallback) {
                            if (fallback.title && fallback.title.length > 0) {
                                title = fallback.title;
                            } else if (fallback.app_id && fallback.app_id.length > 0) {
                                title = fallback.app_id;
                            }
                        }
                    }

                    break;
                }

                i = i + 1;
            }
            
            focusedWindowTitle = title;
        }

        function rebuildVisible() {
            var out = [];
            var i = 0;

            while (i < workspaces.length) {
                var ws = workspaces[i];

                out.push({
                    "id": ws.id,
                    "idx": ws.idx,
                    "name": ws.name,
                    "output": ws.output,
                    "label": labelFor(ws),
                    "focused": !!ws.is_focused,
                    "active": !!ws.is_active,
                    "urgent": !!ws.is_urgent,
                    "occupied": workspaceHasWindows(ws)
                });

                i = i + 1;
            }

            out.sort(function(a, b) {
                var ao = a.output || "";
                var bo = b.output || "";

                if (ao < bo) {
                    return -1;
                }

                if (ao > bo) {
                    return 1;
                }

                return a.idx - b.idx;
            });

            visibleWorkspaces = out;
            updateFocusedWindowTitle();
        }

        function handleEventLine(line) {
            line = line.trim();

            if (line.length === 0) {
                return;
            }

            var ev;

            try {
                ev = JSON.parse(line);
            } catch (e) {
                console.log("niri event parse failed: " + line);
                return;
            }

            if (ev.WorkspacesChanged) {
                workspaces = ev.WorkspacesChanged.workspaces;
                rebuildVisible();
                return;
            }

            if (ev.WorkspaceActivated) {
                var active = ev.WorkspaceActivated;
                var next = [];
                var i = 0;

                while (i < workspaces.length) {
                    var ws = clone(workspaces[i]);

                    if (ws.id === active.id) {
                        ws.is_active = true;
                        ws.is_focused = !!active.focused;
                    } else {
                        if (active.focused) {
                            ws.is_focused = false;
                        }
                    }

                    next.push(ws);
                    i = i + 1;
                }

                workspaces = next;
                rebuildVisible();
                return;
            }

            if (ev.WorkspaceUrgencyChanged) {
                var urg = ev.WorkspaceUrgencyChanged;
                var nextUrg = [];
                var u = 0;

                while (u < workspaces.length) {
                    var uws = clone(workspaces[u]);

                    if (uws.id === urg.id) {
                        uws.is_urgent = !!urg.urgent;
                    }

                    nextUrg.push(uws);
                    u = u + 1;
                }

                workspaces = nextUrg;
                rebuildVisible();
                return;
            }

            if (ev.WorkspaceActiveWindowChanged) {
                var aw = ev.WorkspaceActiveWindowChanged;
                var nextAw = [];
                var a = 0;

                while (a < workspaces.length) {
                    var aws = clone(workspaces[a]);

                    if (aws.id === aw.workspace_id) {
                        aws.active_window_id = aw.active_window_id;
                    }

                    nextAw.push(aws);
                    a = a + 1;
                }

                workspaces = nextAw;
                rebuildVisible();
                return;
            }

            if (ev.WindowsChanged) {
                var byId = {};
                var j = 0;

                while (j < ev.WindowsChanged.windows.length) {
                    var win = ev.WindowsChanged.windows[j];
                    byId[win.id] = win;
                    j = j + 1;
                }

                windowsById = byId;
                rebuildVisible();
                return;
            }

            if (ev.WindowOpenedOrChanged) {
                var changed = ev.WindowOpenedOrChanged.window;
                var newMap = clone(windowsById);

                newMap[changed.id] = changed;
                windowsById = newMap;

                rebuildVisible();
                return;
            }

            if (ev.WindowClosed) {
                var closed = ev.WindowClosed.id;
                var newerMap = clone(windowsById);

                delete newerMap[closed];
                windowsById = newerMap;

                rebuildVisible();
                return;
            }
        }

        function focusWorkspace(ws) {
            var ref = "";

            /* if (/\*ws.name && ws.name.length > 0*\/ false) { */
            /*     ref = ws.name; */
            /* } else { */
            ref = String(ws.idx);
            /* } */

            Quickshell.execDetached(["niri", "msg", "action", "focus-workspace", ref]);
        }
    }

    Process {
        id: eventStream

        command: ["niri", "msg", "--json", "event-stream"]
        running: true

        stdout: SplitParser {
            onRead: function(data) {
                niri.handleEventLine(data);
            }
        }

        onRunningChanged: {
            if (!running) {
                running = true;
            }
        }
    }

    Process {
        id: statsProc

        command: ["./qbar-stats.sh"]

        stdout: SplitParser {
            onRead: function(data) {
                root.statsText = data.trim();
            }
        }
    }

    Timer {
        interval: 3000
        running: true
        repeat: true

        onTriggered: {
            statsProc.running = true;
        }
    }

    Process {
        id: privacyProc

        command: ["./qbar-privacy.py"]

        stdout: SplitParser {
            onRead: function(data) {
                root.privacyText = data.trim();
            }
        }
    }

    Timer {
        interval: 1000
        running: true
        repeat: true

        onTriggered: {
            privacyProc.running = false;
            privacyProc.running = true;
        }
    }

    Component.onCompleted: {
        statsProc.running = true;
        privacyProc.running = true;
    }
    
    PanelWindow {
        id: bar
        
        anchors {
            top: true
            left: true
            right: true
        }

        implicitHeight: root.barHeight
        exclusiveZone: root.barHeight
        aboveWindows: true
        focusable: false

        Rectangle {
            anchors.fill: parent
            color: root.bg
            clip: true

            // Left
            Row {
                id: workspaceRow
                
                anchors.left: parent.left
                anchors.leftMargin: 0
                anchors.verticalCenter: parent.verticalCenter

                height: parent.height
                spacing: 0

                Repeater {
                    model: niri.visibleWorkspaces

                    Rectangle {
                        id: wsItem

                        property var ws: modelData

                        height: root.barHeight
                        width: wsText.implicitWidth + 4

                        color: ws.focused ? root.sel : root.bg

                        Text {
                            id: wsText

                            anchors.centerIn: parent
                            padding: 4

                            text: wsItem.ws.label
                            color: wsItem.ws.urgent
                                ? root.red
                                : wsItem.ws.focused
                                    ? root.fg
                                    : wsItem.ws.occupied
                                        ? root.fg
                                        : root.dim

                            font.family: root.fontName
                            font.pixelSize: root.fontSize
                            renderType: Text.NativeRendering
                            verticalAlignment: Text.AlignVCenter
                        }

                        MouseArea {
                            anchors.fill: parent
                            cursorShape: Qt.PointingHandCursor

                            onClicked: {
                                niri.focusWorkspace(wsItem.ws);
                            }
                        }
                    }
                }
            }

            // Center
            Rectangle {
                id: titleBox

                anchors.left: workspaceRow.right
                anchors.right: trayRow.left
                // anchors.right: notificationLabel.left
                anchors.top: parent.top
                anchors.bottom: parent.bottom

                anchors.leftMargin: 4
                anchors.rightMargin: 4
                /* anchors.verticalCenterOffset: 0 */
                
                color: root.sel
                clip: true
                visible: width > 8

                Text {
                    anchors.left: parent.left
                    anchors.leftMargin: 4
                    anchors.verticalCenter: parent.verticalCenter

                    width: parent.width - 8

                    text: niri.focusedWindowTitle
                    color: root.fg

                    font.family: root.fontName
                    font.pixelSize: root.fontSize
                    renderType: Text.NativeRendering
                    verticalAlignment: Text.AlignVCenter

                    elide: Text.ElideRight
                }
            }

            
            // Right
            /* Text { */
            /*     id: notificationLabel */

            /*     anchors.right: trayRow.left */
            /*     // anchors.rightMargin: 4 */
            /*     anchors.top: parent.top */
            /*     anchors.bottom: parent.bottom */

            /*     visible: root.notificationCount > 0 */

            /*     text: "N" + String(root.notificationCount) + " | " + root.notificationText */
            /*     color: root.fg */

            /*     font.family: root.fontName */
            /*     font.pixelSize: root.fontSize */
            /*     renderType: Text.NativeRendering */

            /*     horizontalAlignment: Text.AlignRight */
            /*     verticalAlignment: Text.AlignVCenter */

            /*     elide: Text.ElideLeft */

            /*     MouseArea { */
            /*         anchors.fill: parent */
            /*         acceptedButtons: Qt.LeftButton | Qt.RightButton */

            /*         onClicked: function(mouse) { */
            /*             if (mouse.button === Qt.RightButton) { */
            /*                 var i = 0; */

            /*                 while (i < notificationServer.trackedNotifications.count) { */
            /*                     var n = notificationServer.trackedNotifications.get(i); */
            /*                     if (n) { */
            /*                         n.dismiss(); */
            /*                     } */
            /*                     i = i + 1; */
            /*                 } */

            /*                 root.notificationCount = 0; */
            /*                 root.notificationText = ""; */
            /*             } */
            /*         } */
            /*     } */
            /* } */
            
            Row {
                id: trayRow

                anchors.right: privacyLabel.left
                anchors.rightMargin: 4
                anchors.top: parent.top
                anchors.bottom: parent.bottom

                spacing: 3

                visible: true

                Repeater {
                    model: SystemTray.items

                    delegate: Item {
                        id: trayItemBox

                        property var trayItem: modelData

                        width: root.barHeight
                        height: root.barHeight

                        IconImage {
                            id: trayIcon

                            anchors.centerIn: parent
                            implicitSize: root.barHeight - 3

                            source: trayItemBox.trayItem ? trayItemBox.trayItem.icon : ""
                            asynchronous: true
                            mipmap: false
                        }

                        Text {
                            anchors.centerIn: parent
                            visible: !trayItemBox.trayItem || trayIcon.source === ""
                            text: "?"
                            color: root.red
                            font.family: root.fontName
                            font.pixelSize: root.fontSize
                        }

                        MouseArea {
                            anchors.fill: parent
                            acceptedButtons: Qt.LeftButton | Qt.RightButton | Qt.MiddleButton

                            onClicked: function(mouse) {
                                if (!trayItemBox.trayItem) {
                                    return;
                                }

                                if (mouse.button === Qt.LeftButton) {
                                    trayItemBox.trayItem.activate();
                                } else if (mouse.button === Qt.RightButton) {
                                    trayItemBox.trayItem.secondaryActivate();
                                } else if (mouse.button === Qt.MiddleButton) {
                                    trayItemBox.trayItem.display(bar, trayItemBox.x, root.barHeight);
                                }
                            }
                        }
                    }
                }
            }

            Text {
                id: privacyLabel

                anchors.right: statsLabel.left
                anchors.rightMargin: 4
                anchors.top: parent.top
                anchors.bottom: parent.bottom

                text: root.privacyText.length > 0 ? root.privacyText + " |" : ""
                color: root.red

                font.family: root.fontName
                font.pixelSize: root.fontSize
                renderType: Text.NativeRendering

                horizontalAlignment: Text.AlignRight
                verticalAlignment: Text.AlignVCenter
            }

            Text {
                id: statsLabel

                anchors.right: clock.left
                anchors.rightMargin: 4
                anchors.top: parent.top
                anchors.bottom: parent.bottom

                text: root.statsText
                color: root.fg

                font.family: root.fontName
                font.pixelSize: root.fontSize
                renderType: Text.NativeRendering

                horizontalAlignment: Text.AlignRight
                verticalAlignment: Text.AlignVCenter

                elide: Text.ElideLeft
            }
            
            
            Text {
                id: clock

                anchors.right: parent.right
                anchors.rightMargin: 4
                anchors.verticalCenter: parent.verticalCenter

                text: Qt.formatDateTime(new Date(), "ddd MM-dd-yy hh:mm")
                color: root.fg

                font.family: root.fontName
                font.pixelSize: root.fontSize
                renderType: Text.NativeRendering
                verticalAlignment: Text.AlignVCenter

                Timer {
                    interval: 1000
                    running: true
                    repeat: true

                    onTriggered: {
                        clock.text = Qt.formatDateTime(new Date(), "ddd MM-dd-yy hh:mm");
                    }
                }
            }
        }
    }
}
