"use strict";

var itemsToReconcile = [];
var itemsMap = new Map();
var vendorsMap = new Map();
[searchQS, limitQS, pageQS] = getPaginationQS("50"); // 50 is default limit
setPaginationQS(searchQS, limitQS, pageQS);
const IDKEY = "purid";
const TBID = "t_items";
const EDIT_FORM = "f_edit_form";
const ADD_FORM = "f_add_form";
const DATALISTS = [
  {dlid: "list_item_vendors", inputId: "f_vendor", isEdit: false},
  {dlid: "list_item_vendorsEdit", inputId: "f_vendorEdit", isEdit: true}
];
const SUBVIEWS = [
  {id: "t_items", displayStyle: "", viewKey: "main"},
  {id: "d_sub_add_form", displayStyle: "flex", viewKey: "add"},
  {id: "d_sub_edit_form", displayStyle: "flex", viewKey: "edit", fn: getQSId(IDKEY, editItemOpen)},
];
const mainOpen = mainOpenWrapper(SUBVIEWS, [IDKEY]);
const confirmDeleteItem = simpleDeleteConfirmationProcess(
  `Are you sure you want to delete this purchase item?`,
  deleteItem);

popSidebar("Purchase");

// init
Promise.all([listItems()])
  .then(() => {
    successAlert("Purchase items view data loading complete.");
    pseudoRoute(SUBVIEWS);
    return populateVendorList();
  })
  .catch(errorAlert);

setFormListenerWrapper(ADD_FORM, addItem);
setFormListenerWrapper(EDIT_FORM, editItem);
setFormListenerWrapper("f_pag_form", (event) => { return updateListQS(listItems, event) });

window.onpopstate = function(event) {
  pseudoRoute(SUBVIEWS);
};

// fns

function listItems() {
  return API.listPurchaseItems(searchQS, limitQS, pageQS)
    .then(popTable)
    .catch(errorAlert);
};

function populateVendorList() {
  return API.listVendors()
    .then((vs) => {
      vendorsMap = new Map(vs.map((v) => [v.name, v]));
      setupVendorDatalists(DATALISTS, vendorsMap);
      popListsInner(DATALISTS, vs);
      return;
    })
    .catch(errorAlert);
};

function popTable(is) {
  const tb = getTableBody(TBID);
  tb.innerHTML = "";
  is.forEach((i) => {
    const nextRow = tb.insertRow(-1);
    const markup = `
      <td>${i.genericName}</td>
      <td>${renderBlankCol(i.autodeskId)}</td>
      <td>${i.name}</td>
      <td>${i.description}</td>
      <td>${i.balanceCategory}</td>
      <td>$${formatPrice(i.cost)}</td>
      <td>${i.markup}</td>
      <td>$${formatPrice(i.price)}</td>
      <td>${i.inventory.onHand}</td>
      <td>${i.inventory.onOrder}</td>
      <td>${i.inventory.minOnHand}</td>
      <td>${renderCustomerVendor(i.vendor.name)}</td>
      <td>${renderBlankCol(i.vendorPartNumber)}</td>
      <td>${renderBlankCol(i.vendorCategory)}</td>
      <td>${renderBlankCol(i.preferredVendor)}</td>
      <td>${renderBlankCol(i.leadTime)}</td>
      <td><button class="table-button" onclick="editItemOpen(${i.id});"><i class="fa fa-pencil"></i></button></td>
      <td><button class="table-button" onclick="confirmDeleteItem(${i.id});"><i class="fa fa-trash"></i></button></td>
    `;
    nextRow.innerHTML = markup;
  });
  itemsMap = new Map(is.map((i) => [i.id, i]));
  itemsToReconcile = is;
  return;
};

function addItemOpen() {
  hide(SUBVIEWS);
  show(SUBVIEWS[1]);
  display(SUBVIEWS[1]);
};

function addItem(event) {
  event.preventDefault();
  event.stopPropagation();
  const fd = new FormData(event.target);
  const baseItem = {
    price: 0.00,
    inventory: {
      id: 0, // default placeholder
      referenceId: 0, // default placeholer
      type: "purchase",
      markup: 1.00, // default placeholder
    },
    isLocked: true,
    markup: 1.00,
    vendor: {
      id: 0, // default placeholder
    },
    leadTime: null,
    autodeskId: null,
  };
  return API.addPurchaseItem(mkItem(fd, baseItem))
    .then(() => {
      event.target.reset();
      successAlert("New purchase item added.");
      mainOpen();
      return listItems();
    })
    .catch(errorAlert);
};

function editItemOpen(iid) {
  if (iid === null || iid === undefined) {
    mainOpen();
    return;
  }
  hide(SUBVIEWS);
  show(SUBVIEWS[2]);
  display(SUBVIEWS[2]);
  setQueryString([[IDKEY, iid]]);
  return fillEditItem(iid);
};

function fillEditItem(iid) {
  const i = itemsMap.get(iid);
  [ {id: "f_nameEdit", val: i.name},
    {id: "f_genericNameEdit", val: i.genericName},
    {id: "f_autodeskIdEdit", val: i.autodeskId},
    {id: "f_descriptionEdit", val: i.description},
    {id: "f_balanceCategoryEdit", val: i.balanceCategory},
    {id: "f_costEdit", val: i.cost},
    {id: "f_invIdEdit", val: i.inventory.id},
    {id: "f_invRefTypeEdit", val: i.inventory.type},
    {id: "f_onHandEdit", val: i.inventory.onHand},
    {id: "f_onOrderEdit", val: i.inventory.onOrder},
    {id: "f_minOnHandEdit", val: i.inventory.minOnHand},
    {id: "f_vendorEdit", val: i.vendor.name},
    {id: "f_vendorCategoryEdit", val: i.vendorCategory},
    {id: "f_vendorPartNumberEdit", val: i.vendorPartNumber},
    {id: "f_vendorNameEdit", val: i.vendor.name},
    {id: "f_vendorIdEdit", val: i.vendor.id},
    {id: "f_vendorCompanyEdit", val: i.vendor.company},
    {id: "f_preferredVendorEdit", val: i.preferredVendor},
    {id: "f_leadTimeEdit", val: i.leadTime},
    {id: "f_markupEdit", val: i.markup},
    {id: "f_idEdit", val: i.id},
  ].forEach(setInputVal);
  return;
};

function editItem(event) {
  event.preventDefault();
  event.stopPropagation();
  const fd = new FormData(event.target);
  const iid = Number.parseInt(fd.get("id"));
  if (Number.isNaN(iid)) {
    errorAlert("Invalid item id provided. Contact system admin.");
    return;
  }
  return API.editPurchaseItem(iid, mkItem(fd))
    .then(() => {
      event.target.reset();
      successAlert("Item edit complete.");
      mainOpen();
      return listItems();
    })
    .catch(errorAlert);
};

function mkItem(fd, baseItem=null) {
  const defObjShape = { price: 0.00, inventory: {}, vendor: {}, isLocked: true };
  const intField = "inventory";
  const baseFields = [
    {f: "name"},
    {f: "genericName"},
    {f: "autodeskId"},
    {f: "description"},
    {f: "balanceCategory"},
    {f: "cost", parser: safeParseDouble},
    {f: "vendorPartNumber"},
    {f: "vendorId", actual: "id", parser: safeParseInt, isNested: true, intField: "vendor"},
    {f: "vendorName", actual: "name", isNested: true, intField: "vendor"},
    {f: "vendorCompany", actual: "company", isNested: true, intField: "vendor"},
    {f: "vendorCategory"},
    {f: "preferredVendor"},
    {f: "leadTime"},
    {f: "onHand", parser: safeParseDouble, isNested: true, intField},
    {f: "onOrder", parser: safeParseDouble, isNested: true, intField},
    {f: "minOnHand", parser: safeParseDouble, isNested: true, intField},
  ];
  const editFields = [
    {f: "id", parser: safeParseInt},
    {f: "invId", actual: "id", parser: safeParseInt, isNested: true, intField},
    {f: "id", actual: "referenceId", parser: safeParseInt, isNested: true, intField},
    {f: "type", isNested: true, intField},
    {f: "markup", parser: safeParseDouble},
  ];
  return mkObj(defObjShape, baseItem, fd, baseFields, editFields);
};

function deleteItem(pid) {
  return API.deletePurchaseItem(pid)
    .then(() => {
      successAlert("Purchase item delete.");
      return listItems();
    })
    .catch(errorAlert);
};

function invReconcile(event) {
  event.preventDefault();
  event.stopPropagation();
  return reconcilePrompt();
};

function reconcilePrompt() {
  closeDialogs();
  if (!Array.isArray(itemsToReconcile) || itemsToReconcile.length === 0) {
    itemsToReconcile = [];
    infoAlert("All purchase items reconciled.");
    return;
  }
  const nexti = itemsToReconcile.pop();
  return promptDialog(`Please enter the hand-counted inventory difference for purchase item: ${nexti.name}. CAUTION: For a counted number LOWER than the ERP, enter a POSITIVE value. For a counted number HIGHER than the ERP, enter a NEGATIVE value, e.g. -1234.`,
    cbReconcile(nexti));
};

function cbReconcile(i) {
  return function(event) {
    event.preventDefault();
    event.stopPropagation();
    closeDialogs();
    const fd = new FormData(event.target);
    event.target.reset();
    const txt = fd.get("prompt");
    if (txt === "") {
      return infoAlert(`Invalid entry: "${txt}". A valid number must be entered.`);
    }
    const [errString, diffVal] = safeParseDouble(txt);
    if (errString !== null) {
      errorAlert(`Parse error for new inventory number: ${errString}.`);
      return;
    }
    infoAlert(`The update amount to be set is: ${i.inventory.onHand - diffVal}.`);
    i.inventory.onHand -= diffVal;
    return API.editInventory(i.inventory.id, i.inventory)
      .then(() => {
        closeDialogs();
        return reconcilePrompt();
      })
      .catch(errorAlert);
  };
};
