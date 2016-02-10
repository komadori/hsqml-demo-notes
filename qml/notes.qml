import QtQuick 2.0
import QtQuick.Window 2.0
import HsQML.Model 1.0

Window {
    width: 800; height: 600;
    title: 'HsQML Notes';
    visible: true;

    MouseArea {
        anchors.fill: parent;
        onDoubleClicked: insertNote(mouse.x, mouse.y, 'New Note');
    }

    Repeater {
        model: AutoListModel { source: notes; mode: AutoListModel.ByKey; }

        Rectangle {
            id: noteView; color: 'yellow';
            width: 100; height: header.height + frontView.contentHeight;
            x: modelData.x; y: modelData.y;
            onXChanged: modelData.x = x; onYChanged: modelData.y = y;

            MouseArea {
                id: header; height: 20; 
                anchors.top: parent.top;
                anchors.left: parent.left; anchors.right: parent.right;
                hoverEnabled: true;
                drag.target: noteView;

                Rectangle {
                    anchors.fill: parent;
                    color: Qt.darker(noteView.color,
                        parent.containsMouse ? 1.2 : 1.1);
                }

                Text {
                    anchors.right: parent.right;
                    anchors.rightMargin: 5;
                    anchors.verticalCenter: parent.verticalCenter;
                    font.pixelSize: parent.height;
                    text: '\u2716';
                    color: closeArea.containsMouse ? 'red' : 'black';

                    MouseArea {
                        id: closeArea;
                        anchors.fill: parent;
                        hoverEnabled: true;
                        onClicked: deleteNote(modelData);
                    }
                }
            }

            TextEdit {
                id: frontView;
                anchors.top: header.bottom;
                anchors.left: parent.left; anchors.right: parent.right;
                textMargin: 2;
                wrapMode: TextEdit.Wrap;

                text: modelData.front;
                onTextChanged: modelData.front = frontView.text;
            }
        }
    }
}
