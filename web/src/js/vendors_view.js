"use strict";

var vendorsMap = new Map();
var currentVendor;
var addressMapVendor = new Map();
var setFn;
[searchQS, limitQS, pageQS] = getPaginationQS("50"); // 50 is default limit
setPaginationQS(searchQS, limitQS, pageQS);
const IDKEY = "vid";
const TBID = "t_vendors";
const EDIT_FORM = "f_edit_vendor";
const ADD_FORM = "f_add_vendor";
const ADDRESS_FORM = "f_set_address";
const SUBVIEWS = [
  {id: "t_vendors", displayStyle: "", viewKey: "main"},
  {id: "d_sub_add_form", displayStyle: "flex", viewKey: "add"},
  {id: "d_sub_edit_form", displayStyle: "flex", viewKey: "edit", fn: getQSId(IDKEY, editVendorOpen)},
  {id: "d_sub_address", displayStyle: "flex", viewKey: "address", fn: getQSId(IDKEY, editAddressOpen)},
];
const mainOpen = mainOpenWrapper(SUBVIEWS, [IDKEY]);
const DATALISTS = [
  {dlid: "list_address", inputId: "f_address", isEdit: false},
];
const setAddressPartial = setAddressWrapper(ADDRESS_FORM, API.setVendorToAddress);

popSidebar("Vendors");

// init
Promise.all([
  listVendors(),
  listAddresses(DATALISTS, addressMapVendor),
  ])
  .then(([hole, newMap]) => {
    addressMapVendor = newMap;
    successAlert("Vendors view data loading complete.");
    pseudoRoute(SUBVIEWS);
  })
  .catch(errorAlert);

setFormListenerWrapper(ADD_FORM, addVendor);
setFormListenerWrapper(EDIT_FORM, editVendor);
setFormListenerWrapper("f_pag_form", (event) => { return updateListQS(listVendors, event) });

window.onpopstate = function(event) {
  pseudoRoute(SUBVIEWS);
};

// fns

function listVendors() {
  return API.listVendors(searchQS, limitQS, pageQS)
    .then(popTable)
    .catch(errorAlert);
};

function popTable(vs) {
  const tb = getTableBody(TBID);
  tb.innerHTML = "";
  vs.forEach((v) => {
    if (v.name !== "_default") {
      const nextRow = tb.insertRow(-1);
      const markup = `
        <td>${v.id}</td>
        <td>${v.name}</td>
        <td>${renderBlankCol(v.company)}</td>
        <td onclick="editVendorOpen(${v.id});"><i class="fa fa-pencil"></i></td>
        <td onclick="editAddressOpen(${v.id});"><i class="fa fa-pencil"></i></td>
      `;
      nextRow.innerHTML = markup;
    }
  });
  vendorsMap = new Map(vs.map((v) => [v.id, v]));
  currentVendor = null;
  return;
};

function addVendorOpen() {
  hide(SUBVIEWS);
  show(SUBVIEWS[1]);
  display(SUBVIEWS[1]);
};

function addVendor(event) {
  event.preventDefault();
  event.stopPropagation();
  const fd = new FormData(event.target);
  const name = fd.get("vendorNameAdd");
  const company = fd.get("vendorCompanyAdd");
  return API.addVendor(mkVendor(0, name, company))
    .then(() => {
      event.target.reset();
      infoAlert("New vendor added.");
      mainOpen();
      return listVendors();
    })
    .catch(errorAlert);
};

function editVendorOpen(vid) {
  if (vid === null || vid === undefined) {
    mainOpen();
    return;
  }
  hide(SUBVIEWS);
  show(SUBVIEWS[2]);
  display(SUBVIEWS[2]);
  setQueryString([[IDKEY, vid]]);
  return fillEditVendor(vid);
};

function fillEditVendor(vid) {
  const v = vendorsMap.get(vid);
  [ {id: "f_nameEdit", val: v.name},
    {id: "f_companyEdit", val: v.company},
    {id: "f_idEdit", val: v.id},
    {id: "f_addressAssignId", val: v.id},
  ].forEach(setInputVal);
  currentVendor = v;
  return;
};

function editVendor(event) {
  event.preventDefault();
  event.stopPropagation();
  const fd = new FormData(event.target);
  const id = Number.parseInt(fd.get("vendorIdEdit"));
  const name = fd.get("vendorNameEdit");
  const company = fd.get("vendorCompanyEdit");
  if (Number.isNaN(id)) {
    errorAlert("Invalid vendor id provided. Contact system admin.");
    return;
  }
  return API.editVendor(id, mkVendor(id, name, company))
    .then(() => {
      event.target.reset();
      infoAlert("Vendor edit complete.");
      mainOpen();
      return listVendors();
    })
    .catch(errorAlert);
};

// TODO duplicated logic...
function editAddressOpen(vid) {
  if (vid === null || vid === undefined) {
    mainOpen();
    return;
  }
  hide(SUBVIEWS);
  show(SUBVIEWS[3]);
  display(SUBVIEWS[3]);
  setQueryString([[IDKEY, vid]]);
  infoAlert("Fetching vendor address...");
  return API.getVendorAddress(vid)
    .then((a) => {
      successAlert("Vendor address retrieved.");
      return fillEditAddress(vid, a);
    })
    .catch(errorAlert);
};

function fillEditAddress(vid, a) {
  if (!a) {
    return;
  }
  const v = vendorsMap.get(vid);
  [ {id: "f_addressAssignId", val: v.id},
    {id: "f_addressId", val: a ? a.id || null : null},
    {id: "f_address", val: makeAddressString(a)},
  ].forEach(setInputVal);
  currentVendor = v;
  const n = document.getElementById(ADDRESS_FORM);
  if (setFn) {
    n.removeEventListener("submit", setFn);
  }
  setFn = setAddressPartial(addressMapVendor);
  n.addEventListener("submit", setFn);
  return;
};

function mkVendor(id, name, company) {
  return { id, name, company };
};
