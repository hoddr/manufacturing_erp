"use strict";

var itemsToReconcile = [];
var itemsMap = new Map();
var materialsMap = new Map();
[searchQS, limitQS, pageQS] = getPaginationQS("100"); // 100 is default limit
setPaginationQS(searchQS, limitQS, pageQS);
const IDKEY = "fabid";
const TBID = "t_items";
const EDIT_FORM = "f_edit_form";
const ADD_FORM = "f_add_form";
const DATALISTS = [
  {dlid: "list_materials", inputId: "f_material", isEdit: false},
  {dlid: "list_materialsEdit", inputId: "f_materialEdit", isEdit: true}
];
const CHECKS = [
  {btnid: "f_isStockButton", inpid: "f_isStock"},
  {btnid: "f_isStockButtonEdit", inpid: "f_isStockEdit"},
];
const SUBVIEWS = [
  {id: "t_items", displayStyle: "", viewKey: "main"},
  {id: "d_sub_add_form", displayStyle: "flex", viewKey: "add"},
  {id: "d_sub_edit_form", displayStyle: "flex", viewKey: "edit", fn: getQSId(IDKEY, editItemOpen)},
];
const mainOpen = mainOpenWrapper(SUBVIEWS, [IDKEY]);

popSidebar("Fabrication");

// init
Promise.all([listItems(), populateMaterialList()])
  .then(() => {
    infoAlert("Fabrication items view data loading complete.");
    pseudoRoute(SUBVIEWS);
  })
  .catch(errorAlert);

setFormListenerWrapper(ADD_FORM, addItem);
setFormListenerWrapper(EDIT_FORM, editItem);
CHECKS.forEach(setClickListenerCheckboxWrapper("Yes", "No"));
setFormListenerWrapper("f_pag_form", (event) => { return updateListQS(listItems, event) });

window.onpopstate = function(event) {
  pseudoRoute(SUBVIEWS);
};

// fns

function listItems() {
  return API.listFabItems(searchQS, limitQS, pageQS)
    .then(popTable)
    .catch(errorAlert);
};

function populateMaterialList() {
  return API.listMaterials()
    .then((ms) => {
      materialsMap = new Map(ms.map((m) => [m.name, m]));
      popListsInner(DATALISTS, ms);
      setupMaterialDatalists(DATALISTS, materialsMap);
    })
    .catch(errorAlert);
};

function popTable(is) {
  return new Promise((resolve, reject) => {
    const tb = getTableBody(TBID);
    tb.innerHTML = "";
    is.forEach((i) => {
      const nextRow = tb.insertRow(-1);
      const markup = `
        <td>${i.name}</td>
        <td>${i.description}</td>
        <td>${i.balanceCategory}</td>
        <td>${i.unitQuantity}</td>
        <td>${i.labor}</td>
        <td>${i.materialName}</td>
        <td>$${formatPrice(i.materialCost)}</td>
        <td>$${formatPrice(i.cost)}</td>
        <td>$${formatPrice(i.price)}</td>
        ${renderIsStockTd(i.isStock)}
        <td>${i.inventory.onHand}</td>
        <td>${i.inventory.onOrder}</td>
        <td>${i.inventory.minOnHand}</td>
        <td onclick="editItemOpen(${i.id});"><i class="fa fa-pencil"></i></td>
      `;
      nextRow.innerHTML = markup;
    });
    itemsMap = new Map(is.map((i) => [i.id, i]));
    itemsToReconcile = is.sort((a, b) => a.inventory.id <= b.inventory.id);
    return resolve();
  });
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
    materialCost: 0.00,
    laborCost: 0.00,
    price: 0.00,
    cost: 0.00,
    inventory: {
      id: 0, // default placeholder
      referenceId: 0, // default placeholer
      type: "purchase",
    },
  };
  return API.addFabItem(mkItem(fd, baseItem))
    .then(() => {
      event.target.reset();
      infoAlert("New fabrication item added.");
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
    {id: "f_descriptionEdit", val: i.description},
    {id: "f_balanceCategoryEdit", val: i.balanceCategory},
    {id: "f_materialEdit", val: i.materialName},
    {id: "f_materialIdEdit", val: i.materialId},
    {id: "f_unitQuantEdit", val: i.unitQuantity},
    {id: "f_laborEdit", val: i.labor},
    {id: "f_isStockEdit", val: i.isStock, isCheckBox: true, onText: "Yes", offText: "No", btnid: CHECKS[1].btnid},
    {id: "f_invIdEdit", val: i.inventory.id},
    {id: "f_invRefTypeEdit", val: i.inventory.type},
    {id: "f_onHandEdit", val: i.inventory.onHand},
    {id: "f_onOrderEdit", val: i.inventory.onOrder},
    {id: "f_minOnHandEdit", val: i.inventory.minOnHand},
    {id: "f_idEdit", val: i.id},
  ].forEach(setInputVal);
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
  return API.editFabItem(iid, mkItem(fd))
    .then(() => {
      event.target.reset();
      infoAlert("Fabrication item edit complete.");
      mainOpen();
      return listItems();
    })
    .catch(errorAlert);
};

function mkItem(fd, baseItem=null) {
  const defObjShape = {
    price: 0.00,
    cost: 0.00,
    materialCost: 0.00,
    laborCost: 0.00,
    inventory: {},
  };
  const intField = "inventory";
  const baseFields = [
    {f: "name"},
    {f: "description"},
    {f: "balanceCategory"},
    {f: "unitQuantity", parser: safeParseDouble},
    {f: "labor", parser: safeParseDouble},
    {f: "materialId", parser: safeParseInt},
    {f: "materialName"},
    {f: "isStock", parser: safeParseBool},
    {f: "onHand", parser: safeParseDouble, isNested: true, intField},
    {f: "onOrder", parser: safeParseDouble, isNested: true, intField},
    {f: "minOnHand", parser: safeParseDouble, isNested: true, intField},
  ];
  const editFields = [
    {f: "id", parser: safeParseInt},
    {f: "invId", actual: "id", parser: safeParseInt, isNested: true, intField},
    {f: "id", actual: "referenceId", parser: safeParseInt, isNested: true, intField},
    {f: "type", isNested: true, intField},
  ];
  return mkObj(defObjShape, baseItem, fd, baseFields, editFields);
};

function renderIsStockTd(isStock) {
  const val = isStock ? "&#10004;" : "--";
  return `
    <td>${val}</td>
  `;
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
    infoAlert("All fabrication items reconciled.");
    return listItems();
  }
  const nexti = itemsToReconcile.pop();
  return promptDialog(`Please enter the hand-counted inventory difference for fabrication item: ${nexti.name}. CAUTION: For a counted number LOWER than the ERP, enter a POSITIVE value. For a counted number HIGHER than the ERP, enter a NEGATIVE value, e.g. -1234.`,
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
