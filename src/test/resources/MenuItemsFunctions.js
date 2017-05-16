function canselEdited() {
    that.discardAllEdits();
}

function deleteTab(target, item) {
    simpleSyS.deleteTab(target, item)
}

function deleteTab1(target, item, item1) {
    simpleSyS.deleteTab(target, item)
}

function editRow() {
    var record = simpleSyS.util.getRowNumSelectedGridRecord(that);
    that.startEditing(record.rowNum, record.colNum);
}

function newRow() {
    that.startEditingNew();
}

function refresh() {
    that.invalidateCache();
}

function removeRow() {
    var record = simpleSyS.util.getRowNumSelectedGridRecord(that);
    that.removeSelectedData();
}

function saveRecords() {

}

function enableDeleteTable() {
    return false;
}