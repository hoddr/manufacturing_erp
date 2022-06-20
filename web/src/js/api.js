// main routes

"use strict";

const API = (function() {
  return {
    // ***** MATERIALS ***** //
    listMaterials: checkCreds(listMaterials),
    // getMaterial: getMaterial,
    addMaterial: checkCreds(addMaterial),
    editMaterial: checkCreds(editMaterial),

    // ***** MAT PURCHASES ***** //
    listMatPurchases: checkCreds(listMatPurchases),
    addMatPurchase: checkCreds(addMatPurchase),
    editMatPurchase: checkCreds(editMatPurchase),
    deleteMatPurchase: checkCreds(deleteMatPurchase),
    listMatPurchaseOptionsForMaterial: checkCreds(listMatPurchaseOptionsForMaterial),

    // ***** INVENTORY ***** //
    listInventory: checkCreds(listInventory),
    editInventory: checkCreds(editInventory),

    // ***** LINE ITEMS ***** //
    toggleExtra: checkCreds(toggleExtra),

    // ***** FAB ITEMS ***** //
    listFabItems: checkCreds(listFabItems),
    addFabItem: checkCreds(addFabItem),
    editFabItem: checkCreds(editFabItem),

    // ***** PURCHASE ITEMS ***** //
    listPurchaseItems: checkCreds(listPurchaseItems),
    addPurchaseItem: checkCreds(addPurchaseItem),
    editPurchaseItem: checkCreds(editPurchaseItem),
    deletePurchaseItem: checkCreds(deletePurchaseItem),

    // ***** ASSEMBLIES ***** //
    listAssemblies: checkCreds(listAssemblies),
    editAssembly: checkCreds(editAssembly),
    editAssemblyPartial: checkCreds(editAssemblyPartial),
    deleteAssembly: checkCreds(deleteAssembly),
    addAssembly: checkCreds(addAssembly),
    getAssemblyById: checkCreds(getAssemblyById),
    deleteSubItem: checkCreds(deleteSubItem),

    // ***** CATEGORIES ***** //
    listCategories: checkCreds(listCategories),

    // ***** ORDERS ***** //
    getOrderById: checkCreds(getOrderById),
    listOrderSkeletons: checkCreds(listOrderSkeletons),
    editOrder: checkCreds(editOrder),
    assignOrderToProject: checkCreds(assignOrderToProject),
    deleteOrder: checkCreds(deleteOrder),
    setOrderToBilled: checkCreds(setOrderToBilled),
    setOrderToNotBilled: checkCreds(setOrderToNotBilled),
    setOrderToComplete: checkCreds(setOrderToComplete),
    setOrderToNotComplete: checkCreds(setOrderToNotComplete),
    swapOrderSection: checkCreds(swapOrderSection),
    progressiveOrderStatus: checkCreds(progressiveOrderStatus),
    getFlangeReport: checkCreds(getFlangeReport),
    getNonCompleteBilledOrders: checkCreds(getNonCompleteBilledOrders),
    getNonBilledOrders: checkCreds(getNonBilledOrders),

    // ***** QUOTES ***** //
    listQuoteSkeletons: checkCreds(listQuoteSkeletons),
    addQuote: checkCreds(addQuote),
    editQuote: checkCreds(editQuote),
    deleteQuote: checkCreds(deleteQuote),
    createProjectFromQuote: checkCreds(createProjectFromQuote),
    createOrderFromQuote: checkCreds(createOrderFromQuote),
    getQuote: checkCreds(getQuote),

    // ***** PROJECTS ***** //
    listProjectSkeletons: checkCreds(listProjectSkeletons),
    listProjectSkeletonsInactive: checkCreds(listProjectSkeletonsInactive),
    listProjects: checkCreds(listProjects),
    editProject: checkCreds(editProject),
    deleteProject: checkCreds(deleteProject),
    getProject: checkCreds(getProject),
    markProjectOrdersAsBilled: checkCreds(markProjectOrdersAsBilled),
    setProjectToActive: checkCreds(setProjectToActive),
    setProjectToInactive: checkCreds(setProjectToInactive),
    listNonCompleteProjectOrders: checkCreds(listNonCompleteProjectOrders),
    checkAndUpdateOrderStatus: checkCreds(checkAndUpdateOrderStatus),
    checkOrderStatus: checkCreds(checkOrderStatus),

    // ***** PROJECT TRACKER ***** //
    getTrackerByOrder: checkCreds(getTrackerByOrder),
    getTrackerBySection: checkCreds(getTrackerBySection),
    getTrackerByProject: checkCreds(getTrackerByProject),

    // ***** PROJECT SECTIONS ***** //
    addProjectSection: checkCreds(addProjectSection),
    editProjectSection: checkCreds(editProjectSection),
    deleteProjectSection: checkCreds(deleteProjectSection),

    // ***** PROJECT STATUS REPORT ***** //
    getProjectStatusReport: checkCreds(getProjectStatusReport),

    // ***** PRICING LISTS ***** //
    listPricingLists: checkCreds(listPricingLists),
    addPricingList: checkCreds(addPricingList),
    editPricingList: checkCreds(editPricingList),
    getPricingList: checkCreds(getPricingList),
    setPricingListItems: checkCreds(setPricingListItems),
    deletePricingListItem: checkCreds(deletePricingListItem),
    deletePricingList: checkCreds(deletePricingList),
    addCustomerPricingListMapping: checkCreds(addCustomerPricingListMapping),
    getPricingListCustomers: checkCreds(getPricingListCustomers),
    removeCustomerPricingListMapping: checkCreds(removeCustomerPricingListMapping),

    // ***** EXTRAS ***** //
    setExtras: checkCreds(setExtras),
    deleteProjectExtra: checkCreds(deleteProjectExtra),

    // ***** CUSTOMER ***** //
    listCustomers: checkCreds(listCustomers),
    addCustomer: checkCreds(addCustomer),
    editCustomer: checkCreds(editCustomer),
    setCustomerToNotTaxExempt: checkCreds(setCustomerToNotTaxExempt),
    setCustomerToTaxExempt: checkCreds(setCustomerToTaxExempt),

    // ***** VENDORS ***** //
    listVendors: checkCreds(listVendors),
    addVendor: checkCreds(addVendor),
    editVendor: checkCreds(editVendor),

    // ***** AUTH ***** //
    login: login,
    logout: checkCreds(logout),

    // ***** USERS ***** //
    addUser: checkCreds(addUser),
    editUser: checkCreds(editUser),
    listUsers: checkCreds(listUsers),
    getUserPermissions: checkCreds(getUserPermissions),
    editUserPermissions: checkCreds(editUserPermissions),

    // ***** PRICING ***** //
    getPricing: checkCreds(getPricing),
    addLabor: checkCreds(addLabor),
    addMarkups: checkCreds(addMarkups),

    // ***** FEEDBACK COMPLAINTS ***** //
    listFeedbackComplaints: checkCreds(listFeedbackComplaints),
    addFeedbackComplaint: checkCreds(addFeedbackComplaint),
    editFeedbackComplaint: checkCreds(editFeedbackComplaint),
    deleteFeedbackComplaint: checkCreds(deleteFeedbackComplaint),

    // ***** ERROR LOG ***** //
    listErrorLogEntries: checkCreds(listErrorLogEntries),
    addErrorLogEntry: checkCreds(addErrorLogEntry),
    editErrorLogEntry: checkCreds(editErrorLogEntry),
    deleteErrorLogEntry: checkCreds(deleteErrorLogEntry),

    // ***** REPORTS ***** //
    getQuoteReport: checkCreds(getQuoteReport),
    getMembrainOrderData: checkCreds(getMembrainOrderData),
    getDetailedOrderData: checkCreds(getDetailedOrderData),
    getDetailedQuoteData: checkCreds(getDetailedQuoteData),
    getPricingListReport: checkCreds(getPricingListReport),
    getBuildAssemblyReport: checkCreds(getBuildAssemblyReport),
    getProjectEstimateReport: checkCreds(getProjectEstimateReport),
    getOrderLineItemDetails: checkCreds(getOrderLineItemDetails),
    genOrderPackingSlip: checkCreds(genOrderPackingSlip),
    getProjectLineItemDetails: checkCreds(getProjectLineItemDetails),

    // ***** PURCHASE ORDERS ***** //
    listPOs: checkCreds(listPOs),
    listClosedPOs: checkCreds(listClosedPOs),
    addPO: checkCreds(addPO),
    editPO: checkCreds(editPO),
    deletePO: checkCreds(deletePO),
    getPOById: checkCreds(getPOById),
    addPOItem: checkCreds(addPOItem),
    removePOItem: checkCreds(removePOItem),
    setQBDone: checkCreds(setQBDone),
    setAsSent: checkCreds(setAsSent),
    setAsReceived: checkCreds(setAsReceived),
    setPOPriceVerified: checkCreds(setPOPriceVerified),
    setPOPriceAdjusted: checkCreds(setPOPriceAdjusted),
    setAsClosed: checkCreds(setAsClosed),
    editPOItem: checkCreds(editPOItem),
    getPORfq: checkCreds(getPORfq),
    getPOReport: checkCreds(getPOReport),
    createBackOrder: checkCreds(createBackOrder),

    // ***** ADDRESSES ***** //
    addAddress: checkCreds(addAddress),
    listAddresses: checkCreds(listAddresses),
    getAddress: checkCreds(getAddress),
    editAddress: checkCreds(editAddress),
    deleteAddress: checkCreds(deleteAddress),
    setCustomerToAddress: checkCreds(setCustomerToAddress),
    setVendorToAddress: checkCreds(setVendorToAddress),
    setCCQuoteToAddress: checkCreds(setCCQuoteToAddress),
    getCustomerAddress: checkCreds(getCustomerAddress),
    getVendorAddress: checkCreds(getVendorAddress),
    getCCQuoteAddress: checkCreds(getCCQuoteAddress),

    // ***** CURB CO ***** //
    listCCQuotes: checkCreds(listCCQuotes),
    addCCQuote: checkCreds(addCCQuote),
    getCCQuoteByPID: checkCreds(getCCQuoteByPID),
    editCCQuote: checkCreds(editCCQuote),
    deleteCCQuote: checkCreds(deleteCCQuote),
    getCCQuoteHTMLReport: checkCreds(getCCQuoteHTMLReport),
    addCurbToQuote: checkCreds(addCurbToQuote),
    editCurbOnQuote: checkCreds(editCurbOnQuote),
    deleteCurbFromQuote: checkCreds(deleteCurbFromQuote),
    setCCQuoteToConfirmed: checkCreds(setCCQuoteToConfirmed),
    setCCQuoteToNotConfirmed: checkCreds(setCCQuoteToNotConfirmed),
    convertCCQuoteToOrder: checkCreds(convertCCQuoteToOrder),
    getCCSalesData: checkCreds(getCCSalesData),
  };

  // ***** MATERIALS ***** //
  function listMaterials(search="", limit="10000", page="0") {
    const offset = cleanOffset(limit, page);
    return fjson(url(`materials?search=${search}&limit=${limit}&offset=${offset}`), {});
  };

  function addMaterial(mat) {
    return fjson(url(`materials`), {
      method: "POST",
      body: JSON.stringify(mat),
      headers: {
        "Content-Type": "application/json",
      },
    });
  };

  function editMaterial(id, mat) {
    return fjson(url(`materials/${id}`), {
      method: "PUT",
      body: JSON.stringify(mat),
      headers: {
        "Content-Type": "application/json",
      },
    });
  };

  // ***** MAT PURCHASES ***** //
  function listMatPurchases(search="", limit="10000", page="1") {
    const offset = cleanOffset(limit, page);
    return fjson(url(`mat-purchases?search=${search}&limit=${limit}&offset=${offset}`), {});
  };

  function addMatPurchase(mp) {
    return fjson(url(`mat-purchases`), {
      method: "POST",
      body: JSON.stringify(mp),
      headers: {
        "Content-Type": "application/json",
      },
    });
  };

  function editMatPurchase(mpid, mp) {
    return fjson(url(`mat-purchases/${mpid}`), {
      method: "PUT",
      body: JSON.stringify(mp),
      headers: {
        "Content-Type": "application/json",
      },
    });
  }

  function deleteMatPurchase(mpid) {
    return fjson(url(`mat-purchases/${mpid}`), {
      method: "DELETE",
    });
  };

  function listMatPurchaseOptionsForMaterial(matid) {
    return fjson(url(`mat-purchases/options/${matid}`), {});
  };

  // ***** INVENTORY ***** //
  function listInventory(search="", limit="10000", page="1") {
    const offset = cleanOffset(limit, page);
    return fjson(url(`inventory?search=${search}&limit=${limit}&offset=${offset}`), {});
  };

  function editInventory(id, i) {
    return fjson(url(`inventory/${id}`), {
      method: "PUT",
      body: JSON.stringify(i),
      headers: {
        "Content-Type": "application/json",
      },
    });
  };

  // ***** LINE ITEMS ***** //
  function toggleExtra(lid, f) {
    return fjson(url(`line-items/${lid}/extras?flag=${f}`), {
      method: "PUT",
    });
  };

  // ***** FAB ITEMS ***** //
  function listFabItems(search="", limit="10000", page="1") {
    const offset = cleanOffset(limit, page);
    return fjson(url(`fab-items?search=${search}&limit=${limit}&offset=${offset}`), {});
  };

  function addFabItem(fi) {
    return fjson(url(`fab-items`), {
      method: "POST",
      body: JSON.stringify(fi),
      headers: {
        "Content-Type": "application/json",
      },
    });
  };

  function editFabItem(id, fi) {
    return fjson(url(`fab-items/${id}`), {
      method: "PUT",
      body: JSON.stringify(fi),
      headers: {
        "Content-Type": "application/json",
      },
    });
  };

  // ***** PURCHASE ITEMS ***** //
  function listPurchaseItems(search="", limit="10000", page="1") {
    const offset = cleanOffset(limit, page);
    return fjson(url(`purchase-items?search=${search}&limit=${limit}&offset=${offset}`), {});
  };

  function addPurchaseItem(item) {
    return fjson(url(`purchase-items`), {
      method: "POST",
      body: JSON.stringify(item),
      headers: {
        "Content-Type": "application/json",
      },
    });
  };

  function editPurchaseItem(id, item) {
    return fjson(url(`purchase-items/${id}`), {
      method: "PUT",
      body: JSON.stringify(item),
      headers: {
        "Content-Type": "application/json",
      },
    });
  };

  function deletePurchaseItem(pid) {
    return fjson(url(`purchase-items/${pid}`), {
      method: "DELETE",
    });
  };

  // ***** ASSEMBLIES ***** //
  function listAssemblies(search="", limit="10000", page="1") {
    const offset = cleanOffset(limit, page);
    return fjson(url(`assemblies?search=${search}&limit=${limit}&offset=${offset}`), {});
  };

  function editAssembly(aid, a) {
    return fjson(url(`assemblies/${aid}`), {
      method: "PUT",
      body: JSON.stringify(a),
      headers: {
        "Content-Type": "application/json",
      },
    });
  };

  function editAssemblyPartial(aid, a) {
    return fjson(url(`assemblies/${aid}/partial`), {
      method: "PUT",
      body: JSON.stringify(a),
      headers: {
        "Content-Type": "application/json",
      },
    });
  };

  function addAssembly(a) {
    return fjson(url(`assemblies`), {
      method: "POST",
      body: JSON.stringify(a),
      headers: {
        "Content-Type": "application/json",
      },
    });
  };

  function deleteAssembly(aid) {
    return fjson(url(`assemblies/${aid}`), {
      method: "DELETE",
    });
  };

  function getAssemblyById(aid) {
    return fjson(url(`assemblies/${aid}`), {});
  };

  function deleteSubItem(aid, sid) {
    return fjson(url(`assemblies/${aid}/subItems/${sid}`), {
      method: "DELETE",
    });
  };

  // ***** CATEGORIES ***** //
  function listCategories() {
    return fjson(url(`categories`), {});
  };

  // ***** ORDERS ***** //
  function getOrderById(oid) {
    return fjson(url(`orders/${oid}`), {});
  };

  function listOrderSkeletons(search="", limit="10000", page="0") {
    const offset = cleanOffset(limit, page);
    return fjson(url(`orders/skeleton?search=${search}&limit=${limit}&offset=${offset}`), {});
  };

  function editOrder(oid, ord) {
    return fjson(url(`orders/${oid}`), {
      method: "PUT",
      body: JSON.stringify(ord),
      headers: {
        "Content-Type": "application/json",
      },
    });
  };

  function assignOrderToProject(oid, pid, sid) {
    return fjson(url(`projects/${pid}/sections/${sid}/orders/${oid}`), {
      method: "PUT",
    });
  };

  function deleteOrder(oid) {
    return fjson(url(`orders/${oid}`), {
      method: "DELETE",
    });
  };

  function setOrderToBilled(oid, invoiceNumber) {
    return fjson(url(`orders/${oid}/bill/${invoiceNumber}`), {
      method: "PUT",
    });
  };

  function setOrderToNotBilled(oid) {
    return fjson(url(`orders/${oid}/notbill`), {
      method: "PUT",
    });
  };

  function setOrderToComplete(oid) {
    return fjson(url(`orders/${oid}/complete`), {
      method: "PUT",
    });
  };

  function setOrderToNotComplete(oid) {
    return fjson(url(`orders/${oid}/notcomplete`), {
      method: "PUT",
    });
  };

  function swapOrderSection(pid, oid, sid) {
    return fjson(url(`projects/${pid}/orders/${oid}/sections/${sid}`), {
      method: "PUT",
    });
  };

  function progressiveOrderStatus(oid, li, isAdd) {
    const urlToUse = url(`progressive/orders/${oid}/${isAdd ? "add" : "subtract"}`);
    return fjson(urlToUse, {
      method: "PUT",
      body: JSON.stringify(li),
      headers: {
        "Content-Type": "application/json",
      },
    });
  };

  function getFlangeReport(oid) {
    return fjson(url(`orders/${oid}/flange-count.csv`), {});
  };

  // ***** QUOTES ***** //
  function listQuoteSkeletons(search="", limit="10000", page="1") {
    const offset = cleanOffset(limit, page);
    return fjson(url(`quotes/skeleton?search=${search}&limit=${limit}&offset=${offset}`), {});
  };

  function addQuote(q) {
    return fjson(url(`quotes`), {
      method: "POST",
      body: JSON.stringify(q),
      headers: {
        "Content-Type": "application/json",
      },
    });
  };

  function editQuote(qid, q) {
    return fjson(url(`quotes/${qid}`), {
      method: "PUT",
      body: JSON.stringify(q),
      headers: {
        "Content-Type": "application/json",
      },
    });
  };

  function deleteQuote(qid) {
    return fjson(url(`quotes/${qid}`), {
      method: "DELETE",
    });
  };

  function createProjectFromQuote(qid, p) {
    return fjson(url(`quotes/${qid}/project`), {
      method: "POST",
      body: JSON.stringify(p),
      headers: {
        "Content-Type": "application/json",
      },
    });
  };

  function createOrderFromQuote(qid, o) {
    return fjson(url(`quotes/${qid}/order`), {
      method: "POST",
      body: JSON.stringify(o),
      headers: {
        "Content-Type": "application/json",
      },
    });
  };

  function getQuote(qid) {
    return fjson(url(`quotes/${qid}`), {});
  };

  // ***** PROJECTS ***** //
  function listProjectSkeletons(search="", limit="10000", page="0") {
    const offset = cleanOffset(limit, page);
    return fjson(url(`projects/skeleton?search=${search}&limit=${limit}&offset=${offset}`), {});
  };

  function listProjectSkeletonsInactive(search="", limit="10000", page="0") {
    const offset = cleanOffset(limit, page);
    return fjson(url(`projects/skeleton/inactive?search=${search}&limits=${limit}&offset=${offset}`), {});
  };

  function listProjects() {
    return fjson(url(`projects`), {});
  };

  function editProject(pid, p) {
    return fjson(url(`projects/${pid}`), {
      method: "PUT",
      body: JSON.stringify(p),
      headers: {
        "Content-Type": "application/json",
      },
    });
  };

  function deleteProject(pid) {
    return fjson(url(`projects/${pid}`), {
      method: "DELETE",
    });
  };

  function getProject(pid) {
    return fjson(url(`projects/${pid}`), {});
  };

  function markProjectOrdersAsBilled(pid) {
    return fjson(url(`projects/${pid}/billed`), {
      method: "POST",
    });
  };

  function setProjectToActive(pid) {
    return fjson(url(`projects/${pid}/active`), {
      method: "POST",
    });
  };

  function setProjectToInactive(pid) {
    return fjson(url(`projects/${pid}/inactive`), {
      method: "POST",
    });
  };

  function listNonCompleteProjectOrders() {
    return fjson(url(`projects/orders/non-complete`), {});
  };

  function getNonCompleteBilledOrders() {
    return fjson(url(`orders/non-complete`), {});
  };

  function getNonBilledOrders() {
    return fjson(url(`orders/non-billed`), {});
  };

  function checkAndUpdateOrderStatus(n) {
    return fjson(url(`projects/orders/${n}/non-complete-check`), {
      method: "POST",
    });
  };

  function checkOrderStatus(n) {
    return fjson(url(`orders/${n}/non-complete-check`), {});
  };

  // ***** PROJECT TRACKER ***** //
  function getTrackerByOrder(pid, oid) {
    return fjson(url(`projects/${pid}/orders/${oid}/tracker`), {});
  };

  function getTrackerBySection(pid, sid) {
    return fjson(url(`projects/${pid}/sections/${sid}/tracker`), {});
  };

  function getTrackerByProject(pid) {
    return fjson(url(`projects/${pid}/tracker`), {});
  };

  // ***** PROJECT SECTIONS ***** //
  function addProjectSection(pid, s) {
    return fjson(url(`projects/${pid}/sections`), {
      method: "POST",
      body: JSON.stringify(s),
      headers: {
        "Content-Type": "application/json",
      },
    });
  };

  function editProjectSection(pid, sid, s) {
    return fjson(url(`projects/${pid}/sections/${sid}`), {
      method: "PUT",
      body: JSON.stringify(s),
      headers: {
        "Content-Type": "application/json",
      },
    });
  };

  function deleteProjectSection(pid, sid) {
    return fjson(url(`projects/${pid}/sections/${sid}`), {
      method: "DELETE",
    });
  };

  // ***** PROJECT STATUS REPORT ***** //
  function getProjectStatusReport() {
    return fetch(url(`projects/status`), {
      headers: {"Authorization": addToken()},
    })
      .then(resBlobHandler(`project_status_report.csv`))
      .catch(err => err);
  };

  // ***** PRICING LISTS ***** //
  function listPricingLists() {
    return fjson(url(`pricing-lists`), {});
  };

  function addPricingList(pl) {
    return fjson(url(`pricing-lists`), {
      method: "POST",
      body: JSON.stringify(pl),
      headers: {
        "Content-Type": "application/json",
      },
    });
  };

  function editPricingList(plid, pl) {
    return fjson(url(`pricing-lists/${plid}`), {
      method: "PUT",
      body: JSON.stringify(pl),
      headers: {
        "Content-Type": "application/json",
      },
    });
  };

  function getPricingList(plid) {
    return fjson(url(`pricing-lists/${plid}`), {});
  };

  function setPricingListItems(plid, items) {
    return fjson(url(`pricing-lists/${plid}/items`), {
      method: "PUT",
      body: JSON.stringify(items),
      headers: {
        "Content-Type": "application/json",
      },
    });
  };

  function deletePricingListItem(plid, pli) {
    return fjson(url(`pricing-lists/${plid}/items`), {
      method: "DELETE",
      body: JSON.stringify(pli),
      headers: {
        "Content-Type": "application/json",
      },
    });
  };

  function deletePricingList(plid) {
    return fjson(url(`pricing-lists/${plid}`), {
      method: "DELETE",
    });
  };

  function addCustomerPricingListMapping(plid, c) {
    return fjson(url(`pricing-lists/${plid}/customers`), {
      method: "POST",
      body: JSON.stringify(c),
      headers: {
        "Content-Type": "application/json",
      },
    });
  };

  function getPricingListCustomers(plid) {
    return fjson(url(`pricing-lists/${plid}/customers`), {});
  };

  function removeCustomerPricingListMapping(plid, cid) {
    return fjson(url(`pricing-lists/${plid}/customers/${cid}`), {
      method: "DELETE",
    });
  };

  // ***** EXTRAS ***** //
  function setExtras(pid, exs) {
    return fjson(url(`projects/${pid}/extras`), {
      method: "POST",
      body: JSON.stringify(exs),
      headers: {
        "Content-Type": "application/json",
      },
    });
  };

  function deleteProjectExtra(pid, exid) {
    return fjson(url(`projects/${pid}/extras/${exid}`), {
      method: "DELETE",
    });
  };

  // ***** CUSTOMERS ***** //
  function listCustomers(search="", limit="10000", page="1") {
    const offset = cleanOffset(limit, page);
    return fjson(url(`customers?search=${search}&limit=${limit}&offset=${offset}`), {});
  };

  function addCustomer(c) {
    return fjson(url(`customers`), {
      method: "POST",
      body: JSON.stringify(c),
      headers: {
        "Content-Type": "application/json",
      },
    });
  };

  function editCustomer(id, c) {
    return fjson(url(`customers/${id}`), {
      method: "PUT",
      body: JSON.stringify(c),
      headers: {
        "Content-Type": "application/json",
      },
    });
  };

  function setCustomerToNotTaxExempt(id) {
    return fjson(url(`customers/${id}/tax-exempt-off`), {
      method: "PUT",
    });
  };

  function setCustomerToTaxExempt(id) {
    return fjson(url(`customers/${id}/tax-exempt-on`), {
      method: "PUT",
    });
  };

  // ***** VENDORS ***** //
  function listVendors(search="", limit="10000", page="1") {
    const offset = cleanOffset(limit, page);
    return fjson(url(`vendors?search=${search}&limit=${limit}&offset=${offset}`), {});
  };

  function addVendor(v) {
    return fjson(url(`vendors`), {
      method: "POST",
      body: JSON.stringify(v),
      headers: {
        "Content-Type": "application/json",
      },
    });
  };

  function editVendor(id, v) {
    return fjson(url(`vendors/${id}`), {
      method: "PUT",
      body: JSON.stringify(v),
      headers: {
        "Content-Type": "application/json",
      },
    });
  };

  // ***** AUTH ***** //
  function checkCreds(cb) {
    return function caller(...args) {
      const tk = retrieveTokenCreds(); // will redirect to login if not available
      if (tk) {
        return cb(...args);
      }
      return;
    };
  };

  function login(u, p) {
    return new Promise((resolve, reject) => {
      const encoded = window.btoa(`${u}:${p}`);
      return fetch(url("login"), {
        method: "PUT",
        headers: {
          "Authorization": `Basic ${encoded}`,
        },
      })
        .then((res) => {
          if (!res.ok) {
            return reject(res.statusText);
          }
          return res.json()
            .then((token) => {
              setTokenCreds(token);
              return resolve();
            });
        })
        .catch(errorAlert);
    });
  };

  function logout(event) {
    window.localStorage.setItem("CREDENTIALS_FLUSH", "foobar");
    window.localStorage.removeItem("CREDENTIALS_FLUSH");
    event.preventDefault();
    return fetch(url("logout"), {
      method: "PUT",
      headers: {
        "Authorization": addToken(),
      },
    })
      .then((res) => {
        removeTokenCreds();
        goTo(url("views/login.html"));
        return false;
      })
      .catch(() => {
        removeTokenCreds();
        goTo(url("views/login.html"));
        return false;
      });
  };

  // ***** USERS ***** //
  function addUser(u) {
    return fjson(url(`users`), {
      method: "POST",
      body: JSON.stringify(u),
      headers: {
        "Content-Type": "application/json",
      },
    });
  };

  function editUser(uid, u) {
    return fjson(url(`users/${uid}`), {
      method: "PUT",
      body: JSON.stringify(u),
      headers: {
        "Content-Type": "application/json",
      },
    });
  };

  function listUsers() {
    return fjson(url(`users`), {});
  };

  function getUserPermissions(uid) {
    return fjson(url(`users/${uid}/permissions`), {});
  };

  function editUserPermissions(uid, up) {
    return fjson(url(`users/${uid}/permissions`), {
      method: "PUT",
      body: JSON.stringify(up),
      headers: {
        "Content-Type": "application/json",
      },
    });
  };

  // ***** PRICING ***** //
  function getPricing() {
    return fjson(url(`markups`), {});
  };

  function addLabor(l) {
    return fjson(url(`markups/labor`), {
      method: "POST",
      body: JSON.stringify(l),
      headers: {
        "Content-Type": "application/json",
      },
    });
  };

  function addMarkups(rs) {
    return fjson(url(`markups/margins`), {
      method: "POST",
      body: JSON.stringify(rs),
      headers: {
        "Content-Type": "application/json",
      },
    });
  };

  // ***** FEEDBACK COMPLAINTS ***** //
  function listFeedbackComplaints(search="", limit="10000", page="1") {
    const offset = cleanOffset(limit, page);
    return fjson(url(`feedback-complaints?search=${search}&limit=${limit}&offset=${offset}`), {});
  };

  function addFeedbackComplaint(fc) {
    return fjson(url(`feedback-complaints`), {
      method: "POST",
      body: JSON.stringify(fc),
      headers: {
        "Content-Type": "application/json",
      },
    });
  };

  function editFeedbackComplaint(fcid, fc) {
    return fjson(url(`feedback-complaints/${fcid}`), {
      method: "PUT",
      body: JSON.stringify(fc),
      headers: {
        "Content-Type": "application/json",
      },
    });
  };

  function deleteFeedbackComplaint(fcid) {
    return fjson(url(`feedback-complaints/${fcid}`), {
      method: "DELETE",
    });
  };

  // ***** ERROR LOG ***** //
  function listErrorLogEntries(search="", limit="10000", page="1") {
    const offset = cleanOffset(limit, page);
    return fjson(url(`error-log?search=${search}&limit=${limit}&offset=${offset}`), {});
  };

  function addErrorLogEntry(el) {
    return fjson(url(`error-log`), {
      method: "POST",
      body: JSON.stringify(el),
      headers: {
        "Content-Type": "application/json",
      },
    });
  };

  function editErrorLogEntry(elid, el) {
    return fjson(url(`error-log/${elid}`), {
      method: "PUT",
      body: JSON.stringify(el),
      headers: {
        "Content-Type": "application/json",
      },
    });
  };

  function deleteErrorLogEntry(elid) {
    return fjson(url(`error-log/${elid}`), {
      method: "DELETE",
    });
  };

  // ***** REPORTS ***** //
  function getQuoteReport(qid) {
    return ftext(url(`quotes/${qid}/report`), {})
      .then(htmlToNewTabWindow)
      .catch(err => err);
  };

  function getMembrainOrderData(d) {
    return fetch(url(`orders/membrain-sales`), {
      method: "PUT",
      body: JSON.stringify(d),
      headers: {
        "Authorization": addToken(),
        "Content-Type": "application/json",
      },
    })
      .then(resBlobHandler(`membrain_sales_data.csv`))
      .catch(err => err);
  };

  function getDetailedOrderData(d) {
    return fetch(url(`orders/detailed-sales`), {
      method: "PUT",
      body: JSON.stringify(d),
      headers: {
        "Authorization": addToken(),
        "Content-Type": "application/json",
      },
    })
      .then(resBlobHandler(`detailed_sales_data.csv`))
      .catch(err => err);
  };

  function getDetailedQuoteData(qid) {
    return fetch(url(`quotes/${qid}/detailed-sales`), {
      method: "GET",
      headers: {
        "Authorization": addToken(),
      },
    })
      .then(resBlobHandler(`detailed_quote_data.csv`))
      .catch(err => err);
  };

  function getPricingListReport(plid) {
    return ftext(url(`pricing-lists/${plid}/report`), {})
      .then(htmlToNewTabWindow)
      .catch(err => err);
  };

  function getBuildAssemblyReport(d) {
    return fetch(url(`orders/build-assemblies-report`), {
      method: "PUT",
      body: JSON.stringify(d),
      headers: {
        "Authorization": addToken(),
        "Content-Type": "application/json",
      },
    })
      .then(resBlobHandler(`build_assemblies_report.csv`))
      .catch(err => err);
  };

  function getProjectEstimateReport(quoteId) {
    return ftext(url(`quotes/${quoteId}/project-estimate`), {})
      .then(htmlToNewTabWindow)
      .catch(err => err);
  };

  function getOrderLineItemDetails(oid) {
    return fetch(url(`orders/${oid}/detailed-sales`), {
      method: "GET",
      headers: {
        "Authorization": addToken(),
      },
    })
      .then(resBlobHandler(`${oid}_line_item_details.csv`))
      .catch(err => err);
  };

  function genOrderPackingSlip(oid) {
    return ftext(url(`orders/${oid}/packing-slip`), {})
      .then(htmlToNewTabWindow)
      .catch(err => err);
  };

  function getProjectLineItemDetails(pid) {
    return fetch(url(`projects/${pid}/detailed-sales`), {
      method: "GET",
      headers: {
        "Authorization": addToken(),
      },
    })
      .then(resBlobHandler(`${pid}_line_item_details.csv`))
      .catch(err => err);
  };

  // ***** PURCHASE ORDERS ***** //
  function listPOs(search="", limit="10000", page="1") {
    const offset = cleanOffset(limit, page);
    return fjson(url(`purchase-orders?search=${search}&limit=${limit}&offset=${offset}`), {});
  };

  function listClosedPOs(search="", limit="10000", page="1") {
    const offset = cleanOffset(limit, page);
    return fjson(url(`purchase-orders/closed?search=${search}&limit=${limit}&offset=${offset}`), {});
  };

  function addPO(po) {
    return fjson(url(`purchase-orders`), {
      method: "POST",
      body: JSON.stringify(po),
      headers: {
        "Content-Type": "application/json",
      },
    });
  };

  function editPO(poid, po) {
    return fjson(url(`purchase-orders/${poid}`), {
      method: "PUT",
      body: JSON.stringify(po),
      headers: {
        "Content-Type": "application/json",
      },
    });
  };

  function deletePO(poid) {
    return fjson(url(`purchase-orders/${poid}`), {
      method: "DELETE",
    });
  };

  function getPOById(poid) {
    return fjson(url(`purchase-orders/${poid}`), {});
  };

  function addPOItem(poid, poi) {
    return fjson(url(`purchase-orders/${poid}/items`), {
      method: "POST",
      body: JSON.stringify(poi),
      headers: {
        "Content-Type": "application/json",
      },
    });
  };

  function removePOItem(poid, poiid) {
    return fjson(url(`purchase-orders/${poid}/items/${poiid}`), {
      method: "DELETE",
    });
  };

  function setQBDone(poid) {
    return fjson(url(`purchase-orders/${poid}/qbdone`), {
      method: "PUT",
    });
  };

  function setAsSent(poid) {
    return fjson(url(`purchase-orders/${poid}/as-sent`), {
      method: "PUT",
    });
  };

  function setAsReceived(poid) {
    return fjson(url(`purchase-orders/${poid}/as-received`), {
      method: "PUT",
    });
  };

  function setPOPriceVerified(poid) {
    return fjson(url(`purchase-orders/${poid}/as-prices-verified`), {
      method: "PUT",
    });
  };

  function setPOPriceAdjusted(poid) {
    return fjson(url(`purchase-orders/${poid}/as-prices-adjusted`), {
      method: "PUT",
    });
  };

  function setAsClosed(poid) {
    return fjson(url(`purchase-orders/${poid}/as-closed`), {
      method: "PUT",
    });
  };

  function editPOItem(poid, poiid, poi) {
    return fjson(url(`purchase-orders/${poid}/items/${poiid}`), {
      method: "PUT",
      body: JSON.stringify(poi),
      headers: {
        "Content-Type": "application/json",
      },
    });
  };

  function getPORfq(poid) {
    return ftext(url(`purchase-orders/${poid}/rfq`), {})
      .then(htmlToNewTabWindow)
      .catch(err => err);
  };

  function getPOReport(poid) {
    return ftext(url(`purchase-orders/${poid}/po`), {})
      .then(htmlToNewTabWindow)
      .catch(err => err);
  };

  function createBackOrder(poid, pbos) {
    return fjson(url(`purchase-orders/${poid}/back-order`), {
      method: "POST",
      body: JSON.stringify(pbos),
      headers: {
        "Content-Type": "application/json",
      },
    });
  };

  // ***** ADDRESSES ***** //
  function addAddress(a) {
    return fjson(url(`addresses`), {
      method: "POST",
      body: JSON.stringify(a),
      headers: {
        "Content-Type": "application/json",
      },
    });
  };

  function listAddresses(search="", limit="10000", page="1") {
    const offset = cleanOffset(limit, page);
    return fjson(url(`addresses?search=${search}&limit=${limit}&offset=${offset}`), {});
  };

  function getAddress(aid) {
    return fjson(url(`addresses/${aid}`), {});
  };

  function editAddress(aid, a) {
    return fjson(url(`addresses/${aid}`), {
      method: "PUT",
      body: JSON.stringify(a),
      headers: {
        "Content-Type": "application/json",
      },
    });
  };

  function deleteAddress(aid) {
    return fjson(url(`addresses/${aid}`), {
      method: "DELETE",
    });
  };

  function setCustomerToAddress(aid, cid) {
    return fjson(url(`addresses/${aid}/customer/${cid}`), {
      method: "POST",
    });
  };

  function setVendorToAddress(aid, vid) {
    return fjson(url(`addresses/${aid}/vendor/${vid}`), {
      method: "POST",
    });
  };

  function setCCQuoteToAddress(aid, qid) {
    return fjson(url(`addresses/${aid}/c-c/${qid}`), {
      method: "POST",
    });
  };

  function getCustomerAddress(cid) {
    return fetch(url(`addresses/customer/${cid}`), {
      headers: {
        "Authorization": addToken(),
      },
    })
      .then((res) => {
        if (!res.ok) {
          return res.json()
            .then((msg) => {
              if (msg.status === 404) {
                closeDialogs();
                infoAlert(msg.msg);
                return {id: 0};
              }
              throw new Error(msg.msg);
            })
            .catch(errorAlert);
        }
        return res.json();
      })
      .catch(errorAlert);
  };

  function getVendorAddress(vid) {
    return fetch(url(`addresses/vendor/${vid}`), {
      headers: {
        "Authorization": addToken(),
      },
    })
      .then((res) => {
        if (!res.ok) {
          return res.json()
            .then((msg) => {
              if (msg.status === 404) {
                closeDialogs();
                infoAlert(msg.msg);
                return {id: 0};
              }
              throw new Error(msg.msg);
            })
            .catch(errorAlert);
        }
        return res.json();
      })
      .catch(errorAlert);
  };

  function getCCQuoteAddress(pid) {
    return fetch(url(`addresses/c-c/${pid}`), {
      headers: {
        "Authorization": addToken(),
      },
    })
      .then((res) => {
        if (!res.ok) {
          return res.json()
            .then((msg) => {
              if (msg.status === 404) {
                closeDialogs();
                infoAlert(msg.msg);
                return {id: 0};
              }
              throw new Error(msg.msg);
            })
            .catch(errorAlert);
        }
        return res.json();
      })
      .catch(errorAlert);
  };

  // ***** CURB CO ***** //
  function listCCQuotes(search="", limit="10000", page="0") {
    const offset = cleanOffset(limit, page);
    return fjson(url(`c-c?search=${search}&limit=${limit}&offset=${offset}`), {});
  };

  function addCCQuote(ccq) {
    return fjson(url(`c-c`), {
      method: "POST",
      body: JSON.stringify(ccq),
      headers: {
        "Content-Type": "application/json",
      },
    });
  };

  function getCCQuoteByPID(pid) {
    return fjson(url(`c-c/pid/${pid}`), {});
  };

  function editCCQuote(qid, ccq) {
    return fjson(url(`c-c/${qid}`), {
      method: "PUT",
      body: JSON.stringify(ccq),
      headers: {
        "Content-Type": "application/json",
      },
    });
  };

  function deleteCCQuote(pid) {
    return fjson(url(`c-c/pid/${pid}`), {
      method: "DELETE",
    });
  };

  function getCCQuoteHTMLReport(pid) {
    return ftext(url(`c-c/pid/${pid}/report`), {})
      .then(htmlToNewTabWindow)
      .catch((err) => { throw err; });
  };

  function addCurbToQuote(pid, c) {
    return fjson(url(`c-c/pid/${pid}/curbs`), {
      method: "POST",
      body: JSON.stringify(c),
      headers: {
        "Content-Type": "application/json",
      },
    });
  };

  function editCurbOnQuote(pid, cid, c) {
    return fjson(url(`c-c/pid/${pid}/curbs/${cid}`), {
      method: "PUT",
      body: JSON.stringify(c),
      headers: {
        "Content-Type": "application/json",
      },
    });
  };

  function deleteCurbFromQuote(pid, cid) {
    return fjson(url(`c-c/pid/${pid}/curbs/${cid}`), {
      method: "DELETE",
    });
  };

  function setCCQuoteToConfirmed(pid) {
    return fjson(url(`c-c/pid/${pid}/is-confirmed`), {
      method: "PUT",
    });
  };

  function setCCQuoteToNotConfirmed(pid) {
    return fjson(url(`c-c/pid/${pid}/not-confirmed`), {
      method: "PUT",
    });
  };

  function convertCCQuoteToOrder(pid, ordNum, ordPO) {
    return fjson(url(`c-c/pid/${pid}/order-number/${ordNum}/po/${ordPO}`), {
      method: "PUT",
    });
  };

  function getCCSalesData(dr) {
    return fjson(url(`c-c/sales-data`), {
      method: "PUT",
      body: JSON.stringify(dr),
      headers: {
        "Content-Type": "application/json",
      },
    });
  };


  // PRIVATE
  function fjson(url, reqObj) {
    if (reqObj.headers) {
      reqObj.headers["Authorization"] = addToken();
    } else {
      reqObj.headers = {"Authorization": addToken()};
    }
    return fetch(url, reqObj)
      .then((res) => {
        if (!res.ok) {
          return res.json()
            .then((msg) => {
              throw new Error(msg.msg);
            });
        }
        // no content
        if (res.status === 204) {
          return;
        }
        return res.json();
      })
  };

  function ftext(url, reqObj) {
    if (reqObj.headers) {
      reqObj.headers["Authorization"] = addToken();
    } else {
      reqObj.headers = {"Authorization": addToken()};
    }
    return fetch(url, reqObj)
      .then((res) => {
        if (!res.ok) {
          if (res.status === 400 || res.status === 401 || res.status === 404) {
            return res.text()
              .then((msg) => {
                throw new Error(msg);
              });
          }
          throw new Error(res.statusText);
          return;
        }
        return res.text();
      });
  };

  function resBlobHandler(fileName) {
    return function(res) {
      if (!res.ok) {
        throw new Error(res.statusText);
      }
      return res.blob()
        .then((blob) => {
          promptDownload(blob, fileName);
          return;
        })
        .catch((err) => err);
    };
  };

  function addToken() {
    const tk = retrieveTokenCreds();
    return `Bearer ${tk}`;
  };

  function logoutWrapper() {
    return function() {
      return checkCreds(logout)();
    };
  };

  function cleanOffset(limit, page) {
    const offset = Number.parseInt(limit) * (Number.parseInt(page) - 1);
    if (offset < 0) {
      return 0;
    }
    return offset;
  };
})();
