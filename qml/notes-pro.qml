import QtQuick 2.0
import QtQuick.Controls 1.1
import QtQuick.Layouts 1.1

ApplicationWindow {
    id: view;
    width: 800; height: 600;
    title: 'HsQML Notes Professional';
    visible: true;

    property var hasNote:
        notesView.model[notesView.currentIndex] !== undefined;
    property var note: hasNote ?
        notesView.model[notesView.currentIndex] : {x:0,y:0,front:''};

    menuBar: MenuBar {
        Menu {
            title: 'File';
            MenuItem {
                text: 'Quit';
                onTriggered: Qt.quit();
            }
        }
    }

    toolBar: ToolBar {
        RowLayout {
            ToolButton {
                text: '\u2795';
                onClicked: insertNote(0, 0, 'New Note');
            }
            ToolButton {
                text: '\u2796';
                onClicked: deleteNote(view.note);
                enabled: view.hasNote;
            }
            Item {
                Layout.fillWidth: true;
            }
        }
    }

    SplitView {
        anchors.fill: parent;

        ScrollView {
            ListView {
                id: notesView;
                model: notes;
                focus: true;
                highlightMoveDuration: 0;
                highlightResizeDuration: 0;
                delegate: Text {
                    width: parent.width;
                    maximumLineCount: 1;
                    elide: Text.ElideRight;
                    text: modelData.front;
                    MouseArea {
                        anchors.fill: parent;
                        onClicked: notesView.currentIndex = index;
                    }
                }
                highlight: Rectangle {
                    color: 'lightsteelblue';
                }
            }
        }

        GridLayout {
            columns: 2;

            Label {
                text: 'X Position';
                Layout.alignment: Qt.AlignRight | Qt.AlignTop;
            }
            TextField {
                Layout.fillWidth: true;
                validator: IntValidator {}
                text: view.note.x;
                onEditingFinished: view.note.x = parseInt(text);
                enabled: view.hasNote;
            }

            Label {
                text: 'Y Position';
                Layout.alignment: Qt.AlignRight | Qt.AlignTop;
            }
            TextField {
                Layout.fillWidth: true;
                validator: IntValidator {}
                text: view.note.y;
                onEditingFinished: view.note.y = parseInt(text);
                enabled: view.hasNote;
            }

            Label {
                text: 'Front Text';
                Layout.alignment: Qt.AlignRight | Qt.AlignTop;
            }
            TextArea {
                id: frontView;
                Layout.fillWidth: true;
                Layout.fillHeight: true;
                text: view.note.front;
                onTextChanged: view.note.front = text;
                enabled: view.hasNote;
            }
        }
    }
}
