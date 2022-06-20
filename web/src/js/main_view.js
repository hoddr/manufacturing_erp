"use strict";

const SUBVIEWS = [
  {id: "d_sub_membrain", displayStyle: "flex", viewKey: "main"},
];
const mainOpen = mainOpenWrapper(SUBVIEWS, []);
const [modal, hideModal, showModal] = setupModal("cc_modal", "cc_span");
const customerLookups = {
  "Member List Price": "mlp",
  "Contractor": "c",
  "Preferred Contractor": "pc",
  "Exclusive Contractor": "ec",
  "Distributor": "d",
  "Preferred Distributor": "pd",
  "Exclusive Distributor": "ed",
  "Managed Services Provider": "msp",
};
var revCustomerLookups = { };
for (const [k, v] of Object.entries(customerLookups)) {
  revCustomerLookups[v] = k;
}
const sizeLookup = { "s": "small", "m": "medium", "l": "large", "t": "total" };

popSidebar("Main");

Promise.all([
  API.getOrderById(12),
])
  .then(() => {
  })
  .catch(errorAlert);

setFormListenerWrapper("f_membrain", getMembrainData);

window.onpopsate = function(event) {
  pseudoRoute(SUBVIEWS);
};

function getMembrainData(event) {
  event.preventDefault();
  event.stopPropagation();
  const n = document.getElementById("f_membrain");
  const fd = new FormData(n);
  const baseData = {
    start: null,
    end: null,
  };
  return API.getMembrainOrderData(mkDates(fd))
    .then(() => {
      successAlert("Membrain order data retrieved.");
      return;
    })
    .catch(errorAlert);
};

function getDetailedSalesData(event) {
  event.preventDefault();
  event.stopPropagation();
  const n = document.getElementById("f_membrain");
  const fd = new FormData(n);
  const baseData = {
    start: null,
    end: null,
  };
  return API.getDetailedOrderData(mkDates(fd))
    .then(() => {
      successAlert("Detailed sales data retrieved.");
      return;
    })
    .catch(errorAlert);
};

function mkDates(fd, baseItem=null) {
  const defObjShape = {};
  const baseFields = [
    {f: "start", parser: safeParseDate},
    {f: "end", parser: safeParseDate},
  ];
  return mkObj(defObjShape, baseItem, fd, baseFields, []);
};

function getBuildAssemblyData(event) {
  event.preventDefault();
  event.stopPropagation();
  const n = document.getElementById("f_membrain");
  const fd = new FormData(n);
  const baseData = {
    start: null,
    end: null,
  };
  return API.getBuildAssemblyReport(mkDates(fd))
    .then(() => {
      successAlert("Build assembly report generated.");
      return;
    })
    .catch(errorAlert);
};

function getCCSalesData(event) {
  event.preventDefault();
  event.stopPropagation();
  const n = document.getElementById("f_membrain");
  const fd = new FormData(n);
  const baseData = {
    start: null,
    end: null,
  };
  return API.getCCSalesData(mkDates(fd))
    .then((d) => {
      successAlert("CC sales data retrieved.");
      const keys = [
        "t", // total
        "s", // small
        "m", // medium
        "l", // large
        // member list price
        "mlp_t",
        "mlp_s",
        "mlp_m",
        "mlp_l",
        // contractor
        "c_t",
        "c_s",
        "c_m",
        "c_l",
        // preferred contractor
        "pc_t",
        "pc_s",
        "pc_m",
        "pc_l",
        // exclusive contractor
        "ec_t",
        "ec_s",
        "ec_m",
        "ec_l",
        // distributor
        "d_t",
        "d_s",
        "d_m",
        "d_l",
        // preferred distributor
        "pd_t",
        "pd_s",
        "pd_m",
        "pd_l",
        // exclusive distributor
        "ed_t",
        "ed_s",
        "ed_m",
        "ed_l",
        // managed services provider
        "msp_t",
        "msp_s",
        "msp_m",
        "msp_l",
      ];
      var o = { fastPassQuant: 0, fastPassTotal: 0 };
      keys.forEach((k) => { o[k] = { price: 0, quant: 0, fastPassQuant: 0 }; });
      d.forEach((ccq) => {
        const isFastPass = ccq.fastPassCost > 0;
        o.fastPassQuant += isFastPass ? 1 : 0;
        o.fastPassTotal += ccq.fastPassCost;
        var key = customerLookups[ccq.customer.type];
        ccq.curbs.forEach((ccc) => {
          const size = ccc.size.toLowerCase();
          const q = ccc.quantity;
          const pe = ccc.priceEach;
          const t = ccc.quantity * ccc.priceEach;
          const sizeEntryKey = size;
          const sizeTypeEntryKey = `${key}_${size}`;
          const typeEntryKey = `${key}_t`;
          const totalEntryKey = "t";
          const keyList = [
            sizeEntryKey,
            sizeTypeEntryKey,
            typeEntryKey,
            totalEntryKey,
          ];
          keyList.forEach((k) => {
            const { price, quant, fastPassQuant } = o[k];
            o[k]["price"] = price + t;
            o[k]["quant"] = quant + q;
            o[k]["fastPassQuant"] = isFastPass ? fastPassQuant + q : fastPassQuant;
          });
        });
      });
      showModal();
      const n = document.getElementById("cc_body");
      var markdown = `
        <h4>Fast Pass Totals</h4>
        <ul>
          <li>Quantity: ${o.fastPassQuant}</li>
          <li>Price: $${o.fastPassTotal}</li>
        </ul>
      `;
      keys.forEach((k) => {
        const { price, quant, fastPassQuant } = o[k];
        markdown += `
          <h4>${keyLookup(k)}</h4>
          <ul>
            <li>Quantity: ${quant}</li>
            <li>Price: $${formatPrice(price)}</li>
            <li>Fast Pass Quantity: ${fastPassQuant}</li>
          </ul>
        `;
      });
      n.innerHTML = markdown;
      return;
    })
    .catch(errorAlert);
};

function keyLookup(k) {
  var displayName = "";
  const [p1, p2] = k.split("_");
  if (p2 === undefined) {
    displayName = sizeLookup[p1].toUpperCase();
  } else {
    displayName = revCustomerLookups[p1];
    displayName += ` - ${sizeLookup[p2].toUpperCase()}`;
  }
  return displayName;
};
