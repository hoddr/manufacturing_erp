"use strict";

var currentPricingList;
var currentItem;
var customersMap = new Map();
var purchasesMap = new Map();
var fabricationsMap = new Map();
var itemsMap = new Map();
var assembliesMap = new Map();
const TBID = "t_items";
const TBID2 = "t_customers";
const FID = "f_add_item";
const FID2 = "f_cust_add";
const CVIEWKEY = "customers";
const PLIDKEY = "plid";

const DATALISTS = [
  {dlid: "list_fabrication", inputId: "f_fabrication", isEdit: false},
  {dlid: "list_purchase", inputId: "f_purchase", isEdit: false},
  {dlid: "list_customers", inputId: "f_customer", isEdit: false},
  {dlid: "list_assembly", inputId: "f_assembly", isEdit: false},
];
const SUBVIEWS = [
  {id: "body_1", displayStyle: "", viewKey: "main"},
  {id: "body_2", displayStyle: "", viewKey: CVIEWKEY, fn: showCustomersOpen},
];
const mainOpen = mainOpenWrapper(SUBVIEWS);

function populateFabricationList() {
  return API.listFabItems()
    .then((fs) => {
      fabricationsMap = new Map(fs.map((f) => [f.name, f]));
      popListsInner([DATALISTS[0]], fs);
    })
    .catch(errorAlert);
};

function populatePurchaseList() {
  return API.listPurchaseItems()
    .then((ps) => {
      purchasesMap = new Map(ps.map((p) => [p.name, p]));
      popListsInner([DATALISTS[1]], ps);
    })
    .catch(errorAlert);
};

function populateCustomerList() {
  return API.listCustomers()
    .then((cs) => {
      customersMap = new Map(cs.map((c) => [c.name, c]));
      popListsInner([DATALISTS[2]], cs);
    })
    .catch(errorAlert);
};

function populateAssemblyList() {
  return API.listAssemblies()
    .then((as) => {
      assembliesMap = new Map(as.map((a) => [a.name, a]));
      popListsInner([DATALISTS[3]], as);
    })
    .catch(errorAlert);
};

const [modal, hideModal, showModal] = setupModal("pricingList_modal", "pricingList_span");

popSidebar();

// init
getPricingList()
  .then(() => {
    pseudoRoute(SUBVIEWS);
    successAlert("Pricing list data retrieved.");
    return Promise.all([
      populateFabricationList(),
      populatePurchaseList(),
      populateCustomerList(),
      populateAssemblyList(),
    ]).catch(errorAlert);
  })
  .catch(errorAlert);

setFormListenerWrapper(FID2, addCustomerToList);

window.onpopstate = function(event) {
  pseudoRoute(SUBVIEWS);
};

// fns

function getPricingList() {
  const mplid = getQSId("plid", id => id)();
  if (!mplid) {
    errorAlert("Pricing list id query param not set. Re-select pricing list from pricing lists table view.");
    currentPricingList = null;
    return;
  }
  return API.getPricingList(mplid)
    .then((pl) => {
      currentPricingList = pl;
      setTableCaption("cap_items", `Pricing List: ${pl.description} &rarr; Items`);
      return popInfo(pl);
    })
    .catch(errorAlert);
};

function popInfo(pl) {
  const tb = getTableBody(TBID);
  tb.innerHTML = "";
  pl.items.forEach((pli) => {
    const nextRow = tb.insertRow(-1);
    const markup = `
      <td>${pli.id}</td>
      <td>${pli.referenceId}</td>
      <td>${pli.referenceName}</td>
      <td>${pli.referenceDescription}</td>
      <td>${pli.referenceType}</td>
      <td>$${formatPrice(pli.fixedPrice)}</td>
      <td><button class="table-button" onclick="confirmDeleteItem(${pli.id});"><i class="fa fa-trash"></i></button></td>
    `;
    nextRow.innerHTML = markup;
  });
  itemsMap = new Map(pl.items.map((pli) => [pli.id, pli]));
  return pl;
};

function setPurchaseItem(event) {
  event.stopPropagation();
  event.preventDefault();
  const fd = new FormData(document.getElementById(FID));
  const mpi = fd.get("purchase");
  if (mpi === undefined || mpi === "") {
    errorAlert("No purchase item currently set.");
    return;
  }
  const mi = purchasesMap.get(mpi);
  return setCurrentItem(event, mi);
};

function setFabricationItem(event) {
  event.stopPropagation();
  event.preventDefault();
  const fd = new FormData(document.getElementById(FID));
  const mfi = fd.get("fabrication");
  if (mfi === undefined || mfi === "") {
    errorAlert("No fabrication item currently set.");
    return;
  }
  const mi = fabricationsMap.get(mfi);
  return setCurrentItem(event, mi);
};

function setAssemblyItem(event) {
  event.stopPropagation();
  event.preventDefault();
  const fd = new FormData(document.getElementById(FID));
  const mai = fd.get("assembly");
  if (mai === undefined || mai === "") {
    errorAlert("No assembly item currently set.");
    return;
  }
  const ma = assembliesMap.get(mai);
  infoAlert("Retrieving assembly sub item data for pricing.");
  return API.getAssemblyById(ma.id)
    .then((a) => {
      return setCurrentItem(event, a);
    })
    .catch(errorAlert);
};

function setCurrentItem(event, mi) {
  if (mi === undefined || mi === null) {
    errorAlert("Item was not found or is not in the supported item list.");
    return;
  }
  currentItem = mi;
  const fieldsToAdjust = [
    {id: "f_referenceId", prop: "inventory", innerProp: "referenceId"},
    {id: "f_referenceType", prop: "inventory", innerProp: "type"},
    {id: "f_referenceName", prop: "inventory", innerProp: "name"},
    {id: "f_referenceDescription", prop: "description"},
    {id: "f_previousPrice", prop: "price"},
  ];
  fieldsToAdjust.forEach(({id, prop, innerProp}) => {
    const inp = document.getElementById(id);
    if (innerProp) {
      inp.value = mi[prop][innerProp];
    } else {
      inp.value = mi[prop];
    }
  });
};

function setPricingListItems(event) {
  event.preventDefault();
  event.stopPropagation();
  const f = document.getElementById(FID);
  const fd = new FormData(f);
  const baseItem = {
    id: 0, // default placeholder
    pricingListId: currentPricingList.id,
  };
  var prevItems = currentPricingList.items;
  const isCopy = prevItems.some((pi) => {
    return pi.referenceName === fd.get("referenceName");
  });
  if (!isCopy) {
    const newItem = mkItem(fd, baseItem);
    prevItems.push(newItem);
  }
  return API.setPricingListItems(currentPricingList.id, prevItems)
    .then(() => {
      infoAlert("Pricing list items updated.");
      f.reset();
      return getPricingList();
    })
    .catch(errorAlert);
};

function mkItem(fd, baseItem=null) {
  const defObjShape = { pricingListId: currentPricingList.id };
  const baseFields = [
    {f: "referenceId", parser: safeParseInt},
    {f: "referenceType"},
    {f: "referenceName"},
    {f: "referenceDescription"},
    {f: "fixedPrice", parser: safeParseDouble},
  ];
  const editFields = [];
  return mkObj(defObjShape, baseItem, fd, baseFields, editFields);
};

// TODO can likely be updated to use the wrapper/helper
function confirmDeleteItem(pliid) {
  closeDialogs();
  return confirmDialog("Are you sure you want to remove this item?",
    cbConfirmDeleteItem(pliid),
    () => {closeDialogs(); infoAlert("Item removal cancelled.");});
};

function cbConfirmDeleteItem(pliid) {
  return function inner() {
    closeDialogs();
    const pli = itemsMap.get(pliid);
    return API.deletePricingListItem(currentPricingList.id, pli)
      .then(() => {
        infoAlert("Item removed from pricing list.");
        return getPricingList();
      })
      .catch(errorAlert);
  };
};

function showCustomersOpen() {
  hide(SUBVIEWS);
  show(SUBVIEWS[1]);
  display(SUBVIEWS[1]);
  setTableCaption("cap_customers", `Pricing List: ${currentPricingList.description} &rarr; Supported Customers`);
  const mplid = getQSId(PLIDKEY, id => id)();
  return API.getPricingListCustomers(mplid)
    .then(popCustomersTable)
    .catch(errorAlert);
};

function popCustomersTable(cs) {
  const tb = getTableBody(TBID2);
  tb.innerHTML = "";
  cs.forEach((c) => {
    const nextRow = tb.insertRow(-1);
    const markup = `
      <td>${c.name}</td>
      <td>${c.company}</td>
      <td>${c.type}</td>
      <td>${c.markup}</td>
      <td><button class="table-button" onclick="removeCustomer(${c.id});"><i class="fa fa-trash"></i></button></td>
    `;
    nextRow.innerHTML = markup;
  });
  return cs;
};

function removeCustomer(cid) {
  const mplid = getQSId(PLIDKEY, id => id)();
  return API.removeCustomerPricingListMapping(mplid, cid)
    .then(() => {
      infoAlert("Customer mapping removed.");
      return showCustomersOpen();
    })
    .catch(errorAlert);
};

function addCustomerToList(event) {
  event.preventDefault();
  event.stopPropagation();
  const fd = new FormData(event.target);
  const customerName = fd.get("customer");
  const mcustomer = customersMap.get(customerName);
  const mplid = getQSId(PLIDKEY, id => id)();
  return API.addCustomerPricingListMapping(mplid, mcustomer)
    .then(() => {
      successAlert("Customer mapped to pricing list.");
      event.target.reset();
      return showCustomersOpen();
    })
    .catch(errorAlert);
};

function getPricingListReport() {
  return API.getPricingListReport(currentPricingList.id)
    .then(() => {
      successAlert("Pricing list report generation successful.");
      return;
    })
    .catch(errorAlert);
};
