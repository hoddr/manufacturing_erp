"use strict";
var sectionsMap = new Map();
var filteredSectionsMap = new Map();
var ordersMap = new Map();
var currentProject;
var currentSection;
var currentOrdId;
const SIDKEY = "secid";
const OIDKEY = "ordid";
const TBID = "t_project";
const TBID_ORDERS = "t_orders";
const TBID_LINE_ITEMS = "t_lineItems";
const TBID_SECTIONS = "t_sections";
const TBID_SECTION_ORDERS = "t_orders_section";
const TB_CAPID = "cap_lineItems";

const SECTION_FORM = "f_section";
const SECTION_EDIT_FORM = "f_section_edit";

const DATALISTS = [
  {dlid: "list_section_swaps", inpid: "f_sectionSwap"},
];
const SUBVIEWS = [
  {id: "t_project", displayStyle: "", viewKey: "main"}, // 0
  {id: "t_lineItems", displayStyle: "", viewKey: "lineItems", fn: routeProjectOrderLineItems}, // 1
  {id: "t_sections", displayStyle: "", viewKey: "sections", fn: showSections}, // 2
  {id: "d_sub_section_form", displayStyle: "flex", viewKey: "addSections", fn: routeAddSectionOpen}, // 3
  {id: "d_sub_section_edit_form", displayStyle: "flex", viewKey: "editSection", fn: routeProjectSectionEditOpen}, // 4
  {id: "t_orders_section", displayStyle: "", viewKey: "sectionOrders", fn: routeProjectSectionOrders}, // 5
  {id: "d_sub_swap_section_form", displayStyle: "flex", viewKey: "swapSection", fn: routeProjectSectionOrders}, // 6
  {id: "t_lineItems", displayStyle: "", viewKey: "lineItemsAll", fn: openLineItemsAll}, // 7
];

const mainOpen = mainOpenWrapper(SUBVIEWS, [SIDKEY, OIDKEY]);
const popOrderTable = popOrderTableWrapper(TBID_ORDERS, true);
const popLineItemTable = popLineItemTableWrapper(TBID_LINE_ITEMS, true);
const confirmOrderDeletion = simpleDeleteConfirmationProcess(
  `Are you sure you want to delete this order and all associated line item entries? All data will be lost.`,
  deleteOrder);
const confirmToggleBilledStatus = confirmToggleBilledStatusWrapper(cbConfirmBilledStatusToggle);
const confirmToggleCompleteStatus = confirmToggleCompleteStatusWrapper(cbConfirmCompleteStatusToggle);

const [modal, hideModal, showModal] = setupModal("tracking_modal", "tracking_span");
const [modal2, hideModal2, showModal2] = setupModal("totals_modal", "totals_span");
const [modal3, hideModal3, showModal3] = setupModal("extras_modal", "extras_span");

popSidebar();

Promise.all([
  getProject(),
])
  .then(() => {
    successAlert("Project sub-view data loading complete.");
    pseudoRoute(SUBVIEWS);
  })
  .catch(errorAlert);

setFormListenerWrapper(SECTION_FORM, addSection);
setFormListenerWrapper(SECTION_EDIT_FORM, editSection);
setFormListenerWrapper("f_section_swap", swapSection);

window.onpopstate = function(event) {
  pseudoRoute(SUBVIEWS);
};

// fns

function getProject() {
  const mprjid = getQSId("prjid", id => id)();
  if (!mprjid) {
    errorAlert("Project id query param not set. Re-select project from projects table view.");
    currentProject = null;
    currentSection = null;
    return;
  }
  return API.getProject(mprjid)
    .then((p) => {
      currentProject = p;
      popInfo(p);
      return p;
    })
    .catch(errorAlert);
};

function popInfo() {
  const tb = getTableBody(TBID);
  tb.innerHTML = "";
  const nextRow = tb.insertRow(-1);
  var totWeight = 0;
  var totBilled = 0.00;
  var totFabbed = 0.00;
  currentProject.orders.forEach((o) => {
    totFabbed += o.total;
    totBilled += o.isBilled ? o.total : 0.00;
    o.lineItems.forEach((li) => {
      totWeight += (!li.isExtra && li.weight) ? (li.weight * li.quantity) : 0.0;
    });
  });
  var totEstCost = 0.00;
  currentProject.sections.forEach((s) => {
    totEstCost += s.price;
  });
  const markup = `
    <td>${currentProject.quoteNumber}</td>
    <td>${currentProject.appId}</td>
    <td>${currentProject.name}</td>
    <td>${currentProject.po}</td>
    <td>${renderCustomerVendor(currentProject.customer.name)}</td>
    <td>$${formatPrice(totEstCost)}</td>
    <td>$${formatPrice(totFabbed)}</td>
    <td>$${formatPrice(totBilled)}</td>
    <td>${formatWeight(currentProject.totalEstimatedWeight)}</td>
    <td>${formatWeight(totWeight)}</td>
    <td>%${(totWeight / currentProject.totalEstimatedWeight * 100).toFixed(2)}</td>
    <td>${currentProject.isActive ? "&#x2714;" : "&times;"}</td>
    <td><button class="table-button" onclick="showSections();">See sections</button></td>
  `;
  nextRow.innerHTML = markup;
  return currentProject;
};

function showSections() {
  hide(SUBVIEWS);
  show(SUBVIEWS[2]);
  display(SUBVIEWS[2]);
  setTableCaption("cap_sections", `Project ${currentProject.appId} (${currentProject.name}) &rarr; Sections`);
  return popSectionsTable();
};

function popSectionsTable() {
  const tb = getTableBody(TBID_SECTIONS);
  tb.innerHTML = "";
  currentProject.sections.forEach((s) => {
    var totWeight = 0;
    var totBilled = 0.00;
    var totFabbed = 0.00;
    s.orders.forEach((o) => {
      totFabbed += o.total;
      totBilled += o.isBilled ? o.total : 0.00;
      o.lineItems.forEach((li) => {
        totWeight += (!li.isExtra && li.weight) ? (li.weight * li.quantity) : 0.0;
      });
    });
    const nextRow = tb.insertRow(-1);
    const markup = `
      <td>${s.id}</td>
      <td>${s.name}</td>
      <td>$${formatPrice(s.price)}</td>
      <td>$${formatPrice(totFabbed)}</td>
      <td>$${formatPrice(totBilled)}</td>
      <td>${formatWeight(s.weight)}</td>
      <td>${formatWeight(totWeight)}</td>
      <td>%${(100.0 * totWeight / s.weight).toFixed(2)}</td>
      <td><button class="table-button" onclick="showSectionTracking(${s.id});">See tracking</button></td>
      <td><button class="table-button" onclick="openSectionOrders(${s.id});">See orders</button></td>
      <td onclick="editSectionOpen(${s.id});"><i class="fa fa-pencil"></i></td>
      <td onclick="confirmSectionDelete(${s.id})(event);"><i class="fa fa-trash"></i></td>
    `;
    nextRow.innerHTML = markup;
  });
  sectionsMap = new Map(currentProject.sections.map((s) => [s.id, s]));
  return;
};

function editSectionOpen(sid) {
  hide(SUBVIEWS);
  show(SUBVIEWS[4]);
  display(SUBVIEWS[4]);
  setQueryString([[SIDKEY, sid]]);
  return fillEditSection(sid);
};

function fillEditSection(sid) {
  const s = sectionsMap.get(sid);
  [ {id: "f_sectionNameEdit", val: s.name},
    {id: "f_sectionEstPriceEdit", val: s.price},
    {id: "f_sectionEstWeightEdit", val: s.weight},
    {id: "f_projectIdEdit", val: s.projectId},
    {id: "f_sectionIdEdit", val: s.id},
  ].forEach(setInputVal);
  return;
};

function confirmSectionDelete(sid) {
  return function inner(event) {
    event.stopPropagation();
    event.preventDefault();
    closeDialogs();
    return confirmDialog("Are you sure you want to delete this section? Any project tracking will be lost.",
      cbConfirmSectionDelete(sid),
      cbCancelSectionDel);
  };
};

function cbConfirmSectionDelete(sid) {
  return function inner(event) {
    event.stopPropagation();
    event.preventDefault();
    closeDialogs();
    return promptDialog(`Please enter: "Yes" in the prompt to confirm deletion.`,
      cbPromptSectionDel(sid));
  };
};

function cbPromptSectionDel(sid) {
  return function inner(event) {
    event.stopPropagation();
    event.preventDefault();
    const fd = new FormData(event.target);
    event.target.reset();
    const txt = fd.get("prompt");
    if (txt !== "Yes") {
      closeDialogs();
      return infoAlert(`Invalid entry: "${txt}": cancelling action.`);
    }
    return API.deleteProjectSection(currentProject.id, sid)
      .then(() => {
        closeDialogs();
        successAlert("Project section deletion successful.");
        mainOpen();
        return getProject();
      })
      .catch((e) => {
        closeDialogs();
        return errorAlert(e);
      });
  };
};

function cbCancelSectionDel() {
  closeDialogs();
  return infoAlert("Section deletion cancelled.");
};

function routeAddSectionOpen() {
  addSectionOpen(null);
  return;
};

function addSectionOpen(event) {
  if (event && event.stopPropagation) {
    event.stopPropagation();
    event.preventDefault();
  }
  hide(SUBVIEWS);
  show(SUBVIEWS[3]);
  display(SUBVIEWS[3]);
  return;
};

function routeProjectSectionEditOpen() {
  const msid = getQSId(SIDKEY, (sid) => sid)();
  var errMsg = null;
  if (currentProject) {
    if (msid !== null && msid !== undefined) {
      return editSectionOpen(msid);
    }
    errMsg = "Section id not set. Re-routing to main view.";
  } else {
    errMsg = "Project not set. Re-select project from project table.";
  }
  if (errMsg) {
    closeDialogs();
    errorAlert(errMsg);
  }
  mainOpen();
};

function addSection(event) {
  event.stopPropagation();
  event.preventDefault();
  const fd = new FormData(event.target);
  const baseItem = {
    id: 0, // default placeholder
    projectId: currentProject.id,
  };
  return API.addProjectSection(currentProject.id, mkSection(fd, baseItem))
    .then(() => {
      event.target.reset();
      successAlert("New project section added.");
      mainOpen();
      return getProject();
    })
    .catch(errorAlert);
};

function editSection(event) {
  event.stopPropagation();
  event.preventDefault();
  const fd = new FormData(event.target);
  const sid = Number.parseInt(fd.get("sectionId"));
  if (Number.isNaN(sid)) {
    errorAlert("Invalid section id provided. Contact system admin.");
    return;
  }
  return API.editProjectSection(currentProject.id, sid, mkSection(fd))
    .then(() => {
      event.target.reset();
      successAlert("Project section edit complete.");
      mainOpen();
      return getProject();
    })
    .catch(errorAlert);
};

function mkSection(fd, baseItem=null) {
  const defObjShape = {};
  const baseFields = [
    {f: "sectionName", actual: "name"},
    {f: "sectionEstPrice", actual: "price", parser: safeParseDouble},
    {f: "sectionEstWeight", actual: "weight", parser: safeParseDouble},
  ];
  const editFields = [
    {f: "projectId", parser: safeParseInt},
    {f: "sectionId", actual: "id", parser: safeParseInt},
  ];
  return mkObj(defObjShape, baseItem, fd, baseFields, editFields);
};

function routeProjectSectionOrders() {
  var errMsg = null;
  const msid = getQSId(SIDKEY, (sid) => sid)();
  if (currentProject) {
    sectionsMap = new Map(currentProject.sections.map((s) => [s.id, s]));
    if (msid !== null && msid !== undefined) {
      return openSectionOrders(msid);
    } else {
      errMsg = "Section id not set. Re-routing to main view.";
    }
  } else {
    errMsg = "Project not set. Re-select project from projects table.";
  }
  if (errMsg) {
    closeDialogs();
    errorAlert(errMsg);
  }
  return mainOpen();
};

function openSectionOrders(sid) {
  hide(SUBVIEWS);
  show(SUBVIEWS[5]);
  display(SUBVIEWS[5]);
  setQueryString([[SIDKEY, sid]]);
  return popSectionOrderTable(sid);
};

function popSectionOrderTable(sid) {
  const s = sectionsMap.get(sid);
  setTableCaption("cap_orders_section", `Project ${currentProject.appId} (${currentProject.name}) &rarr; Section ${s.name} &rarr; Orders`);
  const tb = getTableBody(TBID_SECTION_ORDERS);
  tb.innerHTML = "";
  console.log(s.orders);
  s.orders
    .sort((o1, o2) => Number.parseInt(o2.number) - Number.parseInt(o1.number))
    .forEach((o) => {
      const nextRow = tb.insertRow(-1);
      const markup = `
      <td>${o.type}</td>
      <td>${o.number}</td>
      <td>${o.po}</td>
      <td>${o.customer.name}</td>
      <td>$${formatPrice(o.total)}</td>
      ${renderDateTD(o.createdAt)}
      ${renderCompleteStatusTD(o.isComplete, o.id)}
      ${renderDateTD(o.completeAt)}
      ${renderBilledStatusTD(o.isBilled, o.id)}
      ${renderDateTD(o.billedAt)}
      ${renderNullTD(o.invoiceNumber)}
      <td><button class="table-button" onclick="openLineItems(${o.id});">(&rarr;) View line items</button></td>
      <td><button class="table-button" onclick="openOrderFlags(${o.id});"><i class="fa fa-info-circle"></i></button></td>
      <td><button class="table-button" onclick="showOrderTracking(${o.id});">See tracking</button></td>
      <td><button class="table-button" onclick="openSwapSection(${o.id});"><i class="fa fa-pencil"></i></button></td>
      `;
      nextRow.innerHTML = markup;
  });
  ordersMap = new Map(s.orders.map((o) => [o.id, o]));
  return;
};

function routeProjectOrderLineItems() {
  var errMsg = null;
  const moid = getQSId(OIDKEY, (oid) => oid)();
  if (currentProject) {
    if (moid !== null && moid !== undefined) {
      ordersMap = new Map(currentProject.orders.map((o) => [o.id, o]));
      const o = ordersMap.get(moid);
      if (o !== null && o !== undefined) {
        return openLineItems(o.id);
      } else {
        errMsg = "Could not retrieve project order. Re-routing to main view.";
      }
    } else {
      errMsg = "Order id not set. Re-routing to main view.";
    }
  } else {
    errMsg = "Project not set. Re-routing to main view.";
  }
  closeDialogs();
  errorAlert(errMsg);
  mainOpen();
  return;
};

function openLineItems(oid) {
  hide(SUBVIEWS);
  show(SUBVIEWS[1]);
  display(SUBVIEWS[1]);
  setQueryString([[OIDKEY, oid]]);
  const o = ordersMap.get(oid);
  setTableCaption(TB_CAPID, `Project ${currentProject.appId} (${currentProject.name}) &rarr; Order #: ${o.number} &rarr; Line Items`);
  popLineItemTable(o.lineItems);
  return;
};

function openLineItemsAll() {
  hide(SUBVIEWS);
  show(SUBVIEWS[1]);
  display(SUBVIEWS[1]);
  setQueryString([[OIDKEY, "all"]]);
  setTableCaption(TB_CAPID, `Project ${currentProject.appId} (${currentProject.name}) &rarr; All Line Items`);
  popLineItemTableMultiple();
  return;
};

function popLineItemTableMultiple() {
  const tb = getTableBody(TBID_LINE_ITEMS);
  tb.innerHTML = "";
  currentProject.orders.forEach((o) => {
    o.lineItems.forEach((li) => {
      const nextRow = tb.insertRow(-1);
      const markup = `
        <td>${li.id}</td>
        <td>${li.description}</td>
        <td>${li.category}</td>
        <td>${li.quantity}</td>
        <td>$${formatPrice(li.price)}</td>
        <td>${li.weight}</td>
        ${renderBoolTD(li.isExtra)}
      `;
      nextRow.innerHTML = markup;
    });
  });
  return;
};

function setLineItemTableCaption(s) {
  const n = document.getElementById("cap_lineItems");
  n.innerHTML = `Line Items - ${s}`;
  return;
};

function openOrderFlags(oid) {
  const o = ordersMap.get(oid);
  infoAlert(`
    Was wrapped: ${o.isWrapped ? "Yes" : "No"}<br />
    Is residential swap-out: ${o.isResidentialSO ? "Yes" : "No"}<br />
    Is ship KD: ${o.isShipKD ? "Yes" : "No"}
  `);
  return;
};

function openSwapSection(ordid) {
  currentOrdId = ordid;
  hide(SUBVIEWS);
  show(SUBVIEWS[6]);
  display(SUBVIEWS[6]);
  const msid = getQSId(SIDKEY, (id) => id)();
  const fss = currentProject.sections.filter((s) => s.id !== msid)
  filteredSectionsMap = new Map(fss.map((s) => [s.name, s]));
  popListsInner(DATALISTS, fss);
  return;
};

function swapSection(event) {
  event.preventDefault();
  event.stopPropagation();
  const fd = new FormData(event.target);
  const ms = filteredSectionsMap.get(fd.get("sectionSwapName"));
  if (ms && ms.id) {
    return API.swapOrderSection(currentProject.id, currentOrdId, ms.id)
      .then(() => {
        closeDialogs();
        successAlert("Order section assignment swap successful.");
        event.target.reset();
        return getProject()
          .then(() => {
            routeProjectSectionOrders();
          })
          .catch(errorAlert);
      })
      .catch(errorAlert);
  } else {
    event.target.reset();
    errorAlert(`Invalid section selected: ${fd.get("sectionSwapName")}. Try again.`);
    return;
  }
};

function cbConfirmBilledStatusToggle(oid, isBilled) {
  return function inner() {
    closeDialogs();
    if (isBilled) {
      return API.setOrderToNotBilled(oid)
        .then(() => {
          successAlert("Order set to not billed.");
          return getProject()
            .then(() => {
              return routeProjectSectionOrders();
            })
            .catch(errorAlert);
        })
        .catch(errorAlert);
    }
    return promptDialog(`Please enter the correct invoice number for the order.`, (event) => {
      event.preventDefault();
      event.stopPropagation();
      const fd = new FormData(event.target);
      event.target.reset();
      const txt = fd.get("prompt");
      if (txt === "") {
        closeDialogs();
        infoAlert("Please enter a valid invoice number.");
        return;
      }
      return API.setOrderToBilled(oid, txt)
        .then(() => {
          closeDialogs();
          successAlert("Order set to billed.");
          return getProject()
            .then(() => {
              return routeProjectSectionOrders();
            })
            .catch(errorAlert);
        })
        .catch(errorAlert);
    });
  };
};

function cbConfirmCompleteStatusToggle(oid, isComplete) {
  return function inner() {
    closeDialogs();
    if (isComplete) {
      return API.setOrderToNotComplete(oid)
        .then(() => {
          successAlert("Order set to not complete.");
          return getProject()
            .then(() => {
              return routeProjectSectionOrders();
            })
            .catch(errorAlert);
        })
        .catch(errorAlert);
    }
    return API.setOrderToComplete(oid)
      .then(() => {
        successAlert("Order set to complete.");
        return getProject()
          .then(() => {
            return routeProjectSectionOrders();
          })
          .catch(errorAlert);
      })
      .catch(errorAlert);
  };
};

function showProjectTracking(event) {
  event.preventDefault();
  event.stopPropagation();
  return API.getTrackerByProject(currentProject.id)
    .then(popTrackingModal)
    .catch(errorAlert);
};

function showSectionTracking(sid) {
  return API.getTrackerBySection(currentProject.id, sid)
    .then(popTrackingModal)
    .catch(errorAlert);
};

function showOrderTracking(oid) {
  return API.getTrackerByOrder(currentProject.id, oid)
    .then(popTrackingModal)
    .catch(errorAlert);
};

function popTrackingModal(d) {
  showModal();
  const n = document.getElementById("tracking_body");
  const markdown = `
    <h4>RECTANGULAR</h4>
    <ul>
      <li>Weight (lbs): ${d.rectWeight.toFixed(0)}</li>
      <li>Duct Length (ft): ${(d.rectDuctLength / 12).toFixed(0)}</li>
      <li>Liner (sq ft): ${d.linerSqFeetRect.toFixed(0)}</li>
      <li>Double Wall Weight (lbs): ${d.doubleWallRectWeight.toFixed(0)}</li>
      <li># Fittings: ${d.rectFittingCount.toFixed(0)}</li>
      <li># Flex: ${d.flexRectangular.toFixed(0)}</li>
    </ul>
    <h4>ROUND</h4>
    <ul>
      <li>Weight (lbs): ${d.roundWeight.toFixed(0)}</li>
      <li>Pipe Length (ft): ${(d.roundDuctLength / 12).toFixed(0)}</li>
      <li>Liner (sq ft): ${d.linerSqFeetRound.toFixed(0)}</li>
      <li>Double Wall Weight (lbs): ${d.doubleWallRoundWeight.toFixed(0)}</li>
      <li># Fittings: ${d.roundFittingCount.toFixed(0)}</li>
      <li># Flex: ${d.flexRound.toFixed(0)}</li>
    </ul>
    <h4>OVAL</h4>
    <ul>
      <li>Weight (lbs): ${d.ovalWeight.toFixed(0)}</li>
      <li>Pipe Length (ft): ${(d.ovalPipeLength / 12).toFixed(0)}</li>
      <li>Liner (sq ft): ${d.linerSqFeetOval.toFixed(0)}</li>
      <li>Double Wall Weight (lbs): ${d.doubleWallOvalWeight.toFixed(0)}</li>
      <li># Fittings: ${d.ovalFittingCount.toFixed(0)}</li>
    </ul>
    <h4>MISC</h4>
    <ul>
      <li># HETO: ${d.hetos.toFixed(0)}</li>
      <li># Grille Boxes: ${d.grilleBoxes.toFixed(0)}</li>
    </ul>
  `;
  n.innerHTML = markdown;
};

function deleteOrder(oid) {
  return API.deleteOrder(oid)
    .then(() => {
      closeDialogs();
      successAlert("Order and line items deleted with required inventory adjustments.");
      return getProject()
        .then(() => {
          return routeProjectSectionOrders();
        })
        .catch(errorAlert);
    })
    .catch((e) => {
      closeDialogs();
      return errorAlert(e);
    });
};

function toggleLineItemExtra(lid, isExtra) {
  return API.toggleExtra(lid, !isExtra)
    .then(() => {
      successAlert("Line item extra flag toggled.");
      return getProject()
        .then(() => {
          pseudoRoute(SUBVIEWS);
        })
        .catch(errorAlert);
    })
    .catch(errorAlert);
};

function markOrdersAsBilled(event) {
  event.stopPropagation();
  event.preventDefault();
  closeDialogs();
  return promptDialog(`Please enter: "Yes" in the prompt to confirm closure of billing cycle. This will mark all non-billed orders as billed.`,
    closeBilling);
};

function closeBilling(event) {
  event.stopPropagation();
  event.preventDefault();
  const fd = new FormData(event.target);
  event.target.reset();
  const txt = fd.get("prompt");
  // failure path step 2
  if (txt !== "Yes") {
    closeDialogs();
    return infoAlert(`Invalid entry: "${txt}": canceling action.`);
  }
  closeDialogs();
  return API.markProjectOrdersAsBilled(currentProject.id)
    .then(() => {
      closeDialogs();
      successAlert("Billing cycle closed.");
      return getProject();
    })
    .catch(errorAlert);
};

function getCategoryTotals() {
  var cats = { };
  currentProject.orders.forEach((o) => {
    o.lineItems.forEach((li) => {
      if (cats[li.category]) {
        cats[li.category] += li.price * li.quantity;
      } else {
        cats[li.category] = li.price * li.quantity;
      }
    });
  });
  showModal2();
  const n = document.getElementById("totals_body");
  var md = "";
  Object.entries(cats).forEach(([key, val]) => {
    md += `
      <h2>${key}: $${formatPrice(val)}</h2>
    `;
  });
  n.innerHTML = md;
};

function getCurrentToBillExtras() {
  const extras = currentProject.orders
    .filter((o) => o.isComplete && !o.isBilled)
    .reduce((acc, o) => {
      return acc.concat(
        o.lineItems
         .filter((li) => li.isExtra)
         .map((li) => [li, o.number]));
    }, []);
  showModal3();
  const n = document.getElementById("extras_body");
  var md = "<ul>";
  extras.forEach(([ex, onum]) => {
    md += `
      <li>${ex.description}: ${ex.quantity} @ $${formatPrice(ex.price)} on order ${onum}</li>
    `;
  });
  md += "</ul>";
  n.innerHTML = md;
};

function getProjectLineItemDetails(event) {
  event.preventDefault();
  event.stopPropagation();
  return API.getProjectLineItemDetails(currentProject.id)
    .then(() => {
      successAlert("Line item report generated.");
      return;
    })
    .catch(errorAlert);
};
