"use strict";

var pricingListsMap = new Map();
var currentPricingList;
const PLIDKEY = "plid";
const TBID = "t_pricingLists";
const ADD_FORM = "f_pricing_list";
const EDIT_FORM = "f_pricing_list_edit";
const SUBVIEWS = [
  {id: "t_pricingLists", displayStyle: "", viewKey: "main", fn: () => { currentPricingList = null; }}, // 0
  {id: "d_sub_plAdd_form", displayStyle: "flex", viewKey: "add"}, // 1
  {id: "d_sub_plEdit_form", displayStyle: "flex", viewKey: "edit", fn: getQSId(PLIDKEY, editPricingListOpen)}, // 2
];

const confirmDeletePricingList = simpleDeleteConfirmationProcess(
  `Are you sure you wish to delete this pricing list? All data will be lost.`,
  deletePricingList);
const mainOpenInner = mainOpenWrapper(SUBVIEWS, [PLIDKEY]);
const mainOpen = () => {
  mainOpenInner();
  currentPricingList = null;
};

popSidebar("Pricing Lists");

// init
Promise.all([
  listPricingLists(),
])
  .then(() => {
    successAlert("Pricing lists view data loading complete.");
    pseudoRoute(SUBVIEWS);
  })
  .catch(errorAlert);

setFormListenerWrapper(EDIT_FORM, editPricingList);
setFormListenerWrapper(ADD_FORM, addPricingList);

window.onpopstate = function(event) {
  pseudoRoute(SUBVIEWS);
};

function listPricingLists() {
  return API.listPricingLists()
    .then(popTable)
    .catch(errorAlert);
};

function popTable(plis) {
  const tb = getTableBody(TBID);
  tb.innerHTML = "";
  plis.forEach((pl) => {
    const nextRow = tb.insertRow(-1);
    const markup = `
      <td><a href="<<BASE_URL>>/views/pricing_list_view.html?plid=${pl.id}">${pl.id}</a></td>
      <td>${pl.description}</td>
      <td>${pl.effectiveAsOf}</td>
      <td>${pl.effectiveUntil}</td>
      <td onclick="editPricingListOpen(${pl.id});"><i class="fa fa-pencil"></i></td>
      <td onclick="confirmDeletePricingList(${pl.id});"><i class="fa fa-trash"></i></td>
    `;
    nextRow.innerHTML = markup;
  });
  pricingListsMap = new Map(plis.map((pli) => [pli.id, pli]));
  return;
};

function editPricingListOpen(plid) {
  hide(SUBVIEWS);
  show(SUBVIEWS[2]);
  display(SUBVIEWS[2]);
  setQueryString([[PLIDKEY, plid]]);
  return fillEditPricingList(plid);
};

function fillEditPricingList(plid) {
  const pl = pricingListsMap.get(plid);
  [ {id: "f_customerEdit", val: pl.customer.name},
    {id: "f_descriptionEdit", val: pl.description},
    {id: "f_effectiveAsOfEdit", val: (new Date(pl.effectiveAsOf)).toISOString().slice(0, 10)},
    {id: "f_effectiveUntilEdit", val: (new Date(pl.effectiveUntil)).toISOString().slice(0, 10)},
    {id: "f_idEdit", val: pl.id},
  ].forEach(setInputVal);
  return;
};

function editPricingList(event) {
  event.preventDefault();
  event.stopPropagation();
  const fd = new FormData(event.target);
  const plid = Number.parseInt(fd.get("id"));
  if (Number.isNaN(plid)) {
    errorAlert("Invalid pricing list id provided. Contact system admin.");
    return;
  }
  return API.editPricingList(plid, mkPricingList(fd))
    .then(() => {
      event.target.reset();
      successAlert("Pricing list edit complete.");
      mainOpen();
      return listPricingLists();
    })
    .catch(errorAlert);
};

function mkPricingList(fd, baseItem=null) {
  const defObjShape = { customer: {}, items: [] };
  const baseFields = [
    {f: "description"},
    {f: "effectiveAsOf", parser: safeParseDate},
    {f: "effectiveUntil", parser: safeParseDate},
  ];
  const editFields = [
    {f: "id", parser: safeParseInt},
  ];
  return mkObj(defObjShape, baseItem, fd, baseFields, editFields);
};

function addPricingListOpen(event) {
  event.stopPropagation();
  event.preventDefault();
  hide(SUBVIEWS);
  show(SUBVIEWS[1]);
  display(SUBVIEWS[1]);
  return;
};

function addPricingList(event) {
  event.preventDefault();
  event.stopPropagation();
  const fd = new FormData(event.target);
  const baseItem = {
    id: 0, // defaultPlaceholder
    customer: {},
    items: [],
  };
  return API.addPricingList(mkPricingList(fd, baseItem))
    .then(() => {
      event.target.reset();
      successAlert("New pricing list added.");
      mainOpen();
      return listPricingLists();
    })
    .catch(errorAlert);
};

function deletePricingList(plid) {
  return API.deletePricingList(plid)
    .then(() => {
      infoAlert("Pricing list deletion successful.");
      return listPricingLists();
    })
    .catch(errorAlert);
};
