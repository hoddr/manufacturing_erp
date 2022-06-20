"use strict";

var addressMap = new Map();
var currentAddress;
[searchQS, limitQS, pageQS] = getPaginationQS("100"); // 100 is default limit
setPaginationQS(searchQS, limitQS, pageQS);
const IDKEY = "aid";
const TBID = "t_addresses";
const EDIT_FORM = "f_edit_address";
const ADD_FORM = "f_add_address";
const SUBVIEWS = [
  {id: "t_addresses", displayStyle: "", viewKey: "main"},
  {id: "d_sub_add_form", displayStyle: "flex", viewKey: "add"},
  {id: "d_sub_edit_form", displayStyle: "flex", viewKey: "edit", fn: getQSId(IDKEY, editAddressOpen)},
];
const mainOpenInner = mainOpenWrapper(SUBVIEWS, [IDKEY]);
const confirmAddDeletion = simpleDeleteConfirmationProcess(
  `Are you sure you want to delete this address and all customer/vendor mappings? All data will be lost.`,
  deleteAddress);

function mainOpen() {
  mainOpenInner();
  currentAddress = null;
};

popSidebar("Addresses");

// init
Promise.all([
  listAddresses(),
  ])
  .then(() => {
    successAlert("Address view data loading complete.");
    pseudoRoute(SUBVIEWS);
  })
  .catch(errorAlert);

setFormListenerWrapper("f_add_address", addAddress);
setFormListenerWrapper("f_edit_address", editAddress);
setFormListenerWrapper("f_pag_form", (event) => { return updateListQS(listAddresses, event) });

window.onpopstate = function(event) {
  pseudoRoute(SUBVIEWS);
};

// fns

function listAddresses() {
  return API.listAddresses(searchQS, limitQS, pageQS)
    .then(popAddressTable)
    .catch(errorAlert);
};

function popAddressTable(as) {
  const tb = getTableBody(TBID);
  tb.innerHTML = "";
  as.forEach((a) => {
    const nextRow = tb.insertRow(-1);
    const markup = `
      <td>${a.id}</td>
      <td>${a.street}</td>
      <td>${a.street2}</td>
      <td>${a.city}</td>
      <td>${a.state}</td>
      <td>${a.zip}</td>
      <td onclick="editAddressOpen(${a.id});"><i class="fa fa-pencil"></i></td>
      <td onclick="confirmAddDeletion(${a.id});"><i class="fa fa-trash"></i></td>
    `;
    nextRow.innerHTML = markup;
  });
  addressMap = new Map(as.map((a) => [a.id, a]));
  return;
};

function addAddressOpen() {
  hide(SUBVIEWS);
  show(SUBVIEWS[1]);
  display(SUBVIEWS[1]);
};

function addAddress(event) {
  event.preventDefault();
  event.stopPropagation();
  const fd = new FormData(event.target);
  const baseItem = {
    id: 0, // default placeholder
    customerId: 0, // default placeholder
    vendorId: 0, // default placeholder
  };
  return API.addAddress(mkAddress(fd, baseItem))
    .then(() => {
      event.target.reset();
      successAlert("New address book entry created.");
      mainOpen();
      return listAddresses();
    })
    .catch(errorAlert);
};
function editAddressOpen(aid) {
  if (aid === null || aid === undefined) {
    mainOpen();
    return;
  }
  hide(SUBVIEWS);
  show(SUBVIEWS[2]);
  display(SUBVIEWS[2]);
  setQueryString([[IDKEY, aid]]);
  return fillEditAddress(aid);
};

function fillEditAddress(aid) {
  const a = addressMap.get(aid);
  currentAddress = a;
  var fields = [
    {id: "f_streetEdit", val: a.street},
    {id: "f_street2Edit", val: a.street2},
    {id: "f_cityEdit", val: a.city},
    {id: "f_stateEdit", val: a.state},
    {id: "f_zipEdit", val: a.zip},
    {id: "f_idEdit", val: a.id},
  ];
  fields.forEach(setInputVal);
  return;
};

function editAddress(event) {
  event.preventDefault();
  event.stopPropagation();
  const fd = new FormData(event.target);
  const aid = Number.parseInt(fd.get("id"));
  return API.editAddress(currentAddress.id, mkAddress(fd))
    .then(() => {
      event.target.reset();
      successAlert("Address edit complete.");
      mainOpen();
      return listAddresses();
    })
    .catch(errorAlert);
};

function mkAddress(fd, baseItem=null) {
  const defObjShape = { customerId: null, vendorId: null };
  const baseFields = [
    {f: "street"},
    {f: "street2"},
    {f: "city"},
    {f: "state"},
    {f: "zip"},
  ];
  const editFields = [
    {f: "id", parser: safeParseInt},
  ];
  return mkObj(defObjShape, baseItem, fd, baseFields, editFields);
};

function deleteAddress(aid) {
  return API.deleteAddress(aid)
    .then(() => {
      successAlert("Address entry deleted.");
      return listAddresses();
    })
    .catch(errorAlert);
};
