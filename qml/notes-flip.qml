import QtQuick 2.0
import QtQuick.Window 2.0

Window {
    width: 800; height: 600;
    title: 'HsQML Notes';
    visible: true;

    MouseArea {
        anchors.fill: parent;
        onDoubleClicked: insertNote(mouse.x, mouse.y, 'New Note');
    }

    Component {
        id: noteFace;

        Rectangle {
            id: noteView; color: faceColour;
            width: 100; height: header.height + frontView.contentHeight;

            MouseArea {
                id: header; height: 20; 
                anchors.top: parent.top;
                anchors.left: parent.left; anchors.right: parent.right;
                hoverEnabled: true;
                drag.target: parentView;

                Rectangle {
                    anchors.fill: parent;
                    color: Qt.darker(noteView.color,
                        parent.containsMouse ? 1.2 : 1.1);
                }

                Text {
                    anchors.left: parent.left;
                    anchors.leftMargin: 5;
                    anchors.verticalCenter: parent.verticalCenter;
                    font.pixelSize: parent.height;
                    text: faceName == 'front' ? '\u293E' : '\u293F';
                    color: flipArea.containsMouse ? 'blue' : 'black';

                    MouseArea {
                        id: flipArea;
                        anchors.fill: parent;
                        hoverEnabled: true;
                        onClicked: note.reverse = !note.reverse;
                    }
                }

                Text {
                    anchors.right: parent.right;
                    anchors.rightMargin: 5;
                    anchors.verticalCenter: parent.verticalCenter;
                    font.pixelSize: parent.height;
                    text: "\u2716";
                    color: closeArea.containsMouse ? 'red' : 'black';

                    MouseArea {
                        id: closeArea;
                        anchors.fill: parent;
                        hoverEnabled: true;
                        onClicked: deleteNote(note);
                    }
                }
            }

            TextEdit {
                id: frontView;
                anchors.top: header.bottom;
                anchors.left: parent.left; anchors.right: parent.right;
                textMargin: 2;
                wrapMode: TextEdit.Wrap;

                text: note[faceName];
                onTextChanged: note[faceName] = frontView.text;
            }
        }
    }

    Repeater {
        model: notes;

        Flipable {
            id: flipper;
            x: modelData.x; y: modelData.y;

            front: Loader {
                sourceComponent: noteFace;
                enabled: !modelData.reverse;
                property var note: modelData;
                property var parentView: flipper;
                property string faceName: 'front';
                property string faceColour: 'yellow';
            }
            back: Loader {
                sourceComponent: noteFace;
                enabled: modelData.reverse;
                property var note: modelData;
                property var parentView: flipper;
                property string faceName: 'back';
                property string faceColour: 'pink';
            }
            transform: Rotation {
                origin.x: 50; origin.y: 0;
                axis.x: 0; axis.y: -1; axis.z: 0;
                angle: modelData.reverse ? 180 : 0;
                Behavior on angle {
                    NumberAnimation {
                        duration: 250;
                    }
                }
            }

            Binding {
                target: modelData;
                property: 'x';
                value: flipper.x;
            }
            Binding {
                target: modelData;
                property: 'y';
                value: flipper.y;
            }
        }
    }
}
