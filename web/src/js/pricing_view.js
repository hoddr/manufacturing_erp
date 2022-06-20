"use strict";

var currentPricing;
const TBID = "t_rates";
const SUBVIEWS = [
  {id: "t_rates", displayStyle: "", viewKey: "main"},
  {id: "d_sub_add_labor_form", displayStyle: "flex", viewKey: "updateLabor"},
  {id: "d_sub_add_markups_form", displayStyle: "flex", viewKey: "updateMarkups"},
];
const mainOpen = mainOpenWrapper(SUBVIEWS);
popSidebar("Pricing");

// init
Promise.all([
  getPricing(),
])
  .then(() => {
    successAlert("Pricing view data loading complete.");
    pseudoRoute(SUBVIEWS, mainOpen);
  })
  .catch(errorAlert);

setFormListenerWrapper("f_add_labor_form", addLabor);
setFormListenerWrapper("f_add_markups_form", addMarkups);

window.onpopstate = function(event) {
  pseudoRoute(SUBVIEWS, mainOpen);
};

// fns

function getPricing() {
  return API.getPricing()
    .then(popTable)
    .catch(errorAlert);
};

function popTable(p) {
  const tb = getTableBody(TBID);
  tb.innerHTML = "";
  const nextRow = tb.insertRow(-1);
  const markup = `
    <td>${formatPrice(p.laborRate.rate)}</td>
    <td>${formatPrice(p.laborRate.shopRate)}</td>
    <td>${formatPrice(p.laborRate.overheadRate)}</td>
    <td>${formatPrice(p.markups.purchase)}</td>
    <td>${formatPrice(p.markups.material)}</td>
    <td>${formatPrice(p.markups.rectangular)}</td>
    <td>${formatPrice(p.markups.round)}</td>
    <td>${formatPrice(p.markups.oval)}</td>
    <td>${formatPrice(p.markups.stock)}</td>
    <td>${formatPrice(p.markups.fabrication)}</td>
    <td>${formatPrice(p.markups.assembly)}</td>
    <td>${formatPrice(p.markups.quote)}</td>
    <td>${formatPrice(p.markups.project)}</td>
    <td>${formatPrice(p.markups.order)}</td>
  `;
  nextRow.innerHTML = markup;
  currentPricing = p;
  return;
};

function updateLaborRateOpen(event) {
  event.stopPropagation();
  event.preventDefault();
  hide(SUBVIEWS);
  show(SUBVIEWS[1]);
  display(SUBVIEWS[1]);
  [ {id: "f_shopRate", val: formatPrice(currentPricing.laborRate.shopRate)},
    {id: "f_overheadRate", val: formatPrice(currentPricing.laborRate.overheadRate)},
  ].forEach(setInputVal);
  return;
};

function updateMarkupRatesOpen(event) {
  event.stopPropagation();
  event.preventDefault();
  hide(SUBVIEWS);
  show(SUBVIEWS[2]);
  display(SUBVIEWS[2]);
  [ {id: "f_purchase", val: formatPrice(currentPricing.markups.purchase)},
    {id: "f_fabrication", val: formatPrice(currentPricing.markups.fabrication)},
    {id: "f_rectangular", val: formatPrice(currentPricing.markups.rectangular)},
    {id: "f_round", val: formatPrice(currentPricing.markups.round)},
    {id: "f_oval", val: formatPrice(currentPricing.markups.oval)},
    {id: "f_stock", val: formatPrice(currentPricing.markups.stock)},
    {id: "f_assembly", val: formatPrice(currentPricing.markups.assembly)},
    {id: "f_material", val: formatPrice(currentPricing.markups.material)},
    {id: "f_quote", val: formatPrice(currentPricing.markups.quote)},
    {id: "f_project", val: formatPrice(currentPricing.markups.project)},
    {id: "f_order", val: formatPrice(currentPricing.markups.order)},
  ].forEach(setInputVal);
  return;
};

function addLabor(event) {
  event.stopPropagation();
  event.preventDefault();
  const fd = new FormData(event.target);
  const baseItem = {
    id: 0, // default placeholder
    rate: 1.00, // default placeholder
    isCurrent: true,
    baseUnit: "h",
    addedAt: new Date(),
  };
  return API.addLabor(mkLabor(fd, baseItem))
    .then(() => {
      event.target.reset();
      successAlert("Labor rate updated.");
      mainOpen();
      return getPricing();
    })
    .catch(errorAlert);
};

function mkLabor(fd, baseItem=null) {
  const defObjShape = {};
  const baseFields = [
    {f: "shopRate", parser: safeParseDouble},
    {f: "overheadRate", parser: safeParseDouble},
  ];
  const editFields = [];
  return mkObj(defObjShape, baseItem, fd, baseFields, editFields);
};

function addMarkups(event) {
  event.stopPropagation();
  event.preventDefault();
  const fd = new FormData(event.target);
  console.log(fd);
  const baseItem = {
    id: 0, // default placeholder
    isCurrent: true,
    addedAt: new Date(),
  };
  const myMkup = mkMarkup(fd, baseItem);
  if (!myMkup) return;
  return API.addMarkups(myMkup)
    .then(() => {
      event.target.reset();
      successAlert("Markup rates updated.");
      mainOpen();
      return getPricing();
    })
    .catch(errorAlert);
};

function mkMarkup(fd, baseItem=null) {
  const defObjShape = {};
  const baseFields = [
    {f: "purchase", parser: safeParseDouble},
    {f: "fabrication", parser: safeParseDouble},
    {f: "rectangular", parser: safeParseDouble},
    {f: "round", parser: safeParseDouble},
    {f: "oval", parser: safeParseDouble},
    {f: "stock", parser: safeParseDouble},
    {f: "assembly", parser: safeParseDouble},
    {f: "material", parser: safeParseDouble},
    {f: "quote", parser: safeParseDouble},
    {f: "project", parser: safeParseDouble},
    {f: "order", parser: safeParseDouble},
  ];
  const editFields = [];
  try {
    const newMkup = mkObj(defObjShape, baseItem, fd, baseFields, editFields);
    return newMkup;
  } catch (err) {
    console.error(err);
    errorAlert("Error on parsing. Contact system admin");
    return null;
  }
};
