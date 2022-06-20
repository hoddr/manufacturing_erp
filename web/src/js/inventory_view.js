"use strict";

var inventoryMap = new Map();
[searchQS, limitQS, pageQS] = getPaginationQS("100"); // 100 is default limit
setPaginationQS(searchQS, limitQS, pageQS);
const IDKEY = "invid";
const EDIT_FORM = "f_edit_inventory";
const TBID = "t_inventory";
const SUBVIEWS = [
  {id: "t_inventory", displayStyle: "", viewKey: "main"},
  {id: "d_sub_edit_form", displayStyle: "flex", viewKey: "edit", fn: getQSId(IDKEY, editInventoryOpen)},
];
const mainOpen = mainOpenWrapper(SUBVIEWS, [IDKEY]);

popSidebar("Inventory");

// init
Promise.all([
  listInventory(),
  ])
  .then(() => {
    infoAlert("Inventory view data loading complete.");
    pseudoRoute(SUBVIEWS);
  })
  .catch(errorAlert);

setFormListenerWrapper(EDIT_FORM, editInventory);
setFormListenerWrapper("f_pag_form", (event) => { return updateListQS(listInventory, event) });

window.onpopstate = function(event) {
  pseudoRoute(SUBVIEWS);
};

// fns

function listInventory() {
  return API.listInventory(searchQS, limitQS, pageQS)
    .then(popTable)
    .catch(errorAlert);
};

function popTable(is) {
  const tb = getTableBody(TBID);
  tb.innerHTML = "";
  is.forEach((i) => {
    const nextRow = tb.insertRow(-1);
    const markup = `
      <td>${i.id}</td>
      <td>${renderInvType(i.type)}</td>
      <td>${i.name}</td>
      <td>${i.onHand}</td>
      <td>${i.onOrder}</td>
      <td>${renderBlankCol(i.minOnHand)}</td>
      <td onclick="editInventoryOpen(${i.id});"><i class="fa fa-pencil"></i></td>
    `;
    nextRow.innerHTML = markup;
  });
  inventoryMap = new Map(is.map((i) => [i.id, i]));
  return;
};

function editInventoryOpen(iid) {
  if (iid === null || iid === undefined) {
    mainOpen();
    return;
  }
  hide(SUBVIEWS);
  show(SUBVIEWS[1]);
  display(SUBVIEWS[1]);
  setQueryString([[IDKEY, iid]]);
  return fillInventoryEdit(iid);
};

function fillInventoryEdit(iid) {
  const i = inventoryMap.get(iid);
  setTableCaption("cap_current", `Item: ${i.name}`);
  [ {id: "f_onHandEdit", val: i.onHand},
    {id: "f_onOrderEdit", val: i.onOrder},
    {id: "f_minOnHandEdit", val: i.minOnHand},
    {id: "f_idEdit", val: i.id},
    {id: "f_typeEdit", val: i.type},
    {id: "f_refIdEdit", val: i.referenceId},
  ].forEach(setInputVal);
  return;
};

function editInventory(event) {
  event.preventDefault();
  event.stopPropagation();
  const fd = new FormData(event.target);
  const id = Number.parseInt(fd.get("id"));
  if (Number.isNaN(id)) {
    errorAlert("Invalid inventory id provided. Contact system admin.");
    return;
  }
  return API.editInventory(id, mkInventory(fd))
    .then(() => {
      event.target.reset();
      infoAlert("Inventory edit complete.");
      mainOpen();
      return listInventory();
    })
    .catch(errorAlert);
};

function renderInvType(t) {
  switch(t) {
    case "purchase":
      return "PUR";
    case "material":
      return "MAT";
    case "fabrication":
      return "STOCK/FAB";
    case "assembly":
      return "ASSEMBLY";
    default: " - ";
  }
};

function mkInventory(fd) {
  const baseFields = [
    {f: "id", parser: safeParseInt},
    {f: "type"},
    {f: "referenceId", parser: safeParseInt},
    {f: "onHand", parser: safeParseDouble},
    {f: "onOrder", parser: safeParseDouble},
    {f: "minOnHand", parser: safeParseDouble},
  ];
  var o = {};
  baseFields.forEach(({f, parser=idParse}) => {
    const [errString, val] = parser(fd.get(f));
    if (errString !== null) {
      errorAlert( {msg: errString} );
    }
    o[f] = val;
  });
  return o;
};
