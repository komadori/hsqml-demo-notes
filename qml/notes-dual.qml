import QtQuick 2.0
import QtQuick.Window 2.0

Window {
    Loader {
        source: 'notes.qml';
        onLoaded: item.closing.connect(Qt.quit);
    }
    Loader {
        source: 'notes-pro.qml';
        onLoaded: item.closing.connect(Qt.quit);
    }
}
