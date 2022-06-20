using System;
using System.IO;
using System.Collections.Generic;
using System.Linq;
using System.Globalization;

using Interop.QBFC14;

namespace master {
  public class QBCommon {
    private static Dictionary<string, string> catsMap = new Dictionary<string, string>() {
      { "Warm Air Stock", "Warehouse Item" },
      { "Stock Pull", "Manufactured Stock" },
      { "Rectangular Duct", "Rectangular Duct" },
      { "Round Duct", "Round Duct" },
      { "Oval Duct", "Oval Duct" },
      { "Duct Accessory", "Duct Accessory" },
      { "Misc Charge", "Misc Charge" },
      { "Misc Rectangular", "Misc Rectangular" },
      { "Misc Round Oval", "Misc Round Oval" },
      { "Assembly", "Manufactured Stock" },
      { "Purchased", "Warehouse Item" },
      { "Freight", "FREIGHT" },
      { "Labor", "Labor" },
      { "Curb", "Curb Adapter" },
      { "Fast Pass", "Fast Pass" },
    };

    public static IMsgSetRequest CreateQBReq(QBSessionManager sessMgr) {
      IMsgSetRequest req = sessMgr.CreateMsgSetRequest("US", 13, 0);
      req.Attributes.OnError = ENRqOnError.roeContinue;
      return req;
    }

    public static bool QBConnect(QBSessionManager sessMgr) {
      // this is the authorized application name - DO NOT CHANGE
      sessMgr.OpenConnection("", "QB Script");
      return true;
    }

    public static bool QBSession(QBSessionManager sessMgr) {
      sessMgr.BeginSession("", ENOpenMode.omDontCare);
      return true;
    }

    /*
     * LINE ITEM SALES ORDER
     */

    public static void AddLineItemSalesOrder(QBSessionManager sessMgr, IMsgSetRequest req, List<LineItem> lis) {
      try {
        BuildLineItemSO(req, lis);

        // send req and retrieve res
        Console.WriteLine("QB Line Item SO request built...");
        Console.WriteLine("Sending QB Line Item SO request...");

        IMsgSetResponse res = null;

        try {
          res = sessMgr.DoRequests(req);
        } catch (Exception e) {
          handleError(e, sessMgr);
        }

        Console.WriteLine("QB response received...");

        // for debugging
        Console.WriteLine(res.ToXMLString());
        return;
      } catch (Exception e) {
        handleError(e, sessMgr);
      }
    }

    private static void BuildLineItemSO(IMsgSetRequest req, List<LineItem> lis) {
      ISalesOrderAdd soReq = req.AppendSalesOrderAddRq();
      soReq.CustomerRef.FullName.SetValue(lis[0].Customer);
      soReq.PONumber.SetValue(lis[0].PO);
      soReq.RefNumber.SetValue(lis[0].OrderNumber);
      soReq.IsToBePrinted.SetValue(false);
      soReq.IsToBeEmailed.SetValue(false);
      soReq.IsManuallyClosed.SetValue(false);
      AddSOLineItems(soReq, lis);
      return;
    }

    private static void AddSOLineItems(ISalesOrderAdd soReq, List<LineItem> lis) {
      // at this point, line items should have been sanitized!
      foreach (var nli in lis) {
        var li = nli;
        IORSalesOrderLineAdd elem = soReq.ORSalesOrderLineAddList.Append();
        elem.SalesOrderLineAdd.Quantity.SetValue(li.Quantity);
        if (li.Category == "Stock Pull" && li.Name.StartsWith("J PIPE")) {
          li = nli with { Category = "Round Duct" };
        }
        if (li.Category == "Stock Pull" && li.Name.StartsWith("BOXG10")) {
          li = nli with { Category = "Misc Rectangular" };
        }
        if (li.Category == "Stock Pull" && li.Name.StartsWith("SLEEVE - TUBE")) {
          li = nli with { Category = "Misc Round Oval" };
        }

        string temp;
        string cat;
        if (catsMap.TryGetValue(li.Category, out temp)) {
          cat = temp;
        } else {
          cat = li.Category;
        }

        elem.SalesOrderLineAdd.ItemRef.FullName.SetValue(cat);
        if (li.Name != "") {
          elem.SalesOrderLineAdd.Desc.SetValue(li.Name);
        }
        elem.SalesOrderLineAdd.ORRatePriceLevel.Rate.SetValue(Math.Round(li.PriceEach, 2));
        elem.SalesOrderLineAdd.IsManuallyClosed.SetValue(false);
        IDataExt dataExt = elem.SalesOrderLineAdd.DataExtList.Append();
        dataExt.OwnerID.SetValue("0");
        dataExt.DataExtName.SetValue("LBS");
        dataExt.DataExtValue.SetValue((li.Weight * li.Quantity).ToString("G"));
      }
      return;
    }

    /*
     * SIMPLE SALES ORDER
     */

    public static void AddSimpleSalesOrder(QBSessionManager sessMgr, IMsgSetRequest req, List<SimpleGroup> sgs) {
      try {
        BuildSimpleSO(req, sgs);
        Console.WriteLine("QB Simple SO request built...");
        Console.WriteLine("Sending QB Simple SO request...");

        IMsgSetResponse res = null;

        try {
          res = sessMgr.DoRequests(req);
        } catch (Exception e) {
          handleError(e, sessMgr);
        }
        Console.WriteLine("QB response received...");

        // for debugging
        Console.WriteLine(res.ToXMLString());
        return;
      } catch (Exception e) {
        handleError(e, sessMgr);
      }
    }

    private static void BuildSimpleSO(IMsgSetRequest req, List<SimpleGroup> sgs) {
      ISalesOrderAdd soReq = req.AppendSalesOrderAddRq();
      soReq.CustomerRef.FullName.SetValue(sgs[0].Customer);
      soReq.IsManuallyClosed.SetValue(false);
      soReq.IsToBePrinted.SetValue(false);
      soReq.IsToBeEmailed.SetValue(false);
      soReq.PONumber.SetValue(sgs[0].PO);
      soReq.RefNumber.SetValue(sgs[0].OrderNumber);
      AddSimpleSOLineItems(soReq, sgs);
    }

    private static void AddSimpleSOLineItems(ISalesOrderAdd soReq, List<SimpleGroup> sgs) {
      foreach (var sg in sgs) {
        if (sg.Price > 0.00) {
          IORSalesOrderLineAdd elem = soReq.ORSalesOrderLineAddList.Append();
          elem.SalesOrderLineAdd.Quantity.SetValue(sg.Quantity);
          elem.SalesOrderLineAdd.ItemRef.FullName.SetValue(sg.Type);
          elem.SalesOrderLineAdd.Desc.SetValue(sg.Name);
          elem.SalesOrderLineAdd.IsManuallyClosed.SetValue(false);
          elem.SalesOrderLineAdd.ORRatePriceLevel.Rate.SetValue(Math.Round(sg.Price, 2));
          // adds weight to line item in QB
          IDataExt dataExt = elem.SalesOrderLineAdd.DataExtList.Append();
          dataExt.OwnerID.SetValue("0");
          dataExt.DataExtName.SetValue("LBS");
          dataExt.DataExtValue.SetValue(sg.cfiLBS.ToString("G"));
        }
      }
    }

    /*
     * LINE ITEM ESTIMATE
     */

    public static void AddLineItemQuote(QBSessionManager sessMgr, IMsgSetRequest req, List<LineItem> lis) {
      try {
        BuildLineItemEstimate(req, lis);

        // send req and retrieve res
        Console.WriteLine("QB line item estimate request built...");
        Console.WriteLine("Sending QB estimate request...");

        IMsgSetResponse res = null;

        try {
          res = sessMgr.DoRequests(req);
        } catch (Exception e) {
          handleError(e, sessMgr);
        }

        Console.WriteLine("QB response received...");

        // for debugging
        Console.WriteLine(res.ToXMLString());
        return;
      } catch (Exception e) {
        handleError(e, sessMgr);
      }
    }

    private static void BuildLineItemEstimate(IMsgSetRequest req, List<LineItem> lis) {
      IEstimateAdd estReq = req.AppendEstimateAddRq();
      estReq.CustomerRef.FullName.SetValue(lis[0].Customer);
      estReq.PONumber.SetValue(lis[0].PO);
      estReq.RefNumber.SetValue(lis[0].OrderNumber);
      estReq.IsToBeEmailed.SetValue(false);
      AddQuoteLineItems(estReq, lis);
      return;
    }

    private static void AddQuoteLineItems(IEstimateAdd estReq, List<LineItem> lis) {
      // at this point, line items should have been sanitized!
      foreach (var nli in lis) {
        var li = nli;
        IOREstimateLineAdd elem = estReq.OREstimateLineAddList.Append();
        elem.EstimateLineAdd.Quantity.SetValue(li.Quantity);
        // set jones pipe to round duct category...
        if (li.Category == "Stock Pull" && li.Name.StartsWith("J PIPE")) {
          li = nli with { Category = "Round Duct" };
        }
        if (li.Category == "Stock Pull" && li.Name.StartsWith("BOXG10")) {
          li = nli with { Category = "Misc Rectangular" };
        }
        if (li.Category == "Stock Pull" && li.Name.StartsWith("SLEEVE - TUBE")) {
          li = nli with { Category = "Misc Round Oval" };
        }

        string temp;
        string cat;
        if (catsMap.TryGetValue(li.Category, out temp)) {
          cat = temp;
        } else {
          cat = li.Category;
        }

        elem.EstimateLineAdd.ItemRef.FullName.SetValue(cat);
        if (li.Name != "") {
          elem.EstimateLineAdd.Desc.SetValue(li.Name);
        }
        elem.EstimateLineAdd.ORRate.Rate.SetValue(Math.Round(li.PriceEach, 2));
        IDataExt dataExt = elem.EstimateLineAdd.DataExtList.Append();
        dataExt.OwnerID.SetValue("0");
        dataExt.DataExtName.SetValue("LBS");
        dataExt.DataExtValue.SetValue((li.Weight * li.Quantity).ToString("G"));
      }
    }

    /*
     * SIMPLE ESTIMATE
     */

    public static void AddSimpleQuote(QBSessionManager sessMgr, IMsgSetRequest req, List<SimpleGroup> sgs) {
      try {
        BuildSimpleEstimate(req, sgs);

        // send req and retrieve res
        Console.WriteLine("QB simple estimate request built...");
        Console.WriteLine("Sending QB estimate request...");

        IMsgSetResponse res = null;

        try {
          res = sessMgr.DoRequests(req);
        } catch (Exception e) {
          handleError(e, sessMgr);
        }

        Console.WriteLine("QB response received...");

        // for debugging
        Console.WriteLine(res.ToXMLString());
        return;
      } catch (Exception e) {
        handleError(e, sessMgr);
      }
    }

    private static void BuildSimpleEstimate(IMsgSetRequest req, List<SimpleGroup> sgs) {
      IEstimateAdd estReq = req.AppendEstimateAddRq();
      estReq.CustomerRef.FullName.SetValue(sgs[0].Customer);
      estReq.PONumber.SetValue(sgs[0].PO);
      estReq.RefNumber.SetValue(sgs[0].OrderNumber);
      estReq.IsToBeEmailed.SetValue(false);
      AddQuoteLineItemsSimple(estReq, sgs);
      return;
    }

    private static void AddQuoteLineItemsSimple(IEstimateAdd estReq, List<SimpleGroup> sgs) {
      foreach (var sg in sgs) {
        if (sg.Price > 0.00) {
          IOREstimateLineAdd elem = estReq.OREstimateLineAddList.Append();
          elem.EstimateLineAdd.Quantity.SetValue(sg.Quantity);
          elem.EstimateLineAdd.ItemRef.FullName.SetValue(sg.Type);
          elem.EstimateLineAdd.Desc.SetValue(sg.Name);
          elem.EstimateLineAdd.ORRate.Rate.SetValue(Math.Round(sg.Price, 2));
          // adds weight to line item in QB
          IDataExt dataExt = elem.EstimateLineAdd.DataExtList.Append();
          dataExt.OwnerID.SetValue("0");
          dataExt.DataExtName.SetValue("LBS");
          dataExt.DataExtValue.SetValue(sg.cfiLBS.ToString("G"));
        }
      }
    }

    private static void handleError(Exception e, QBSessionManager sessMgr) {
      Console.WriteLine($"Error occured: {e}.");
      sessMgr.EndSession();
      sessMgr.CloseConnection();
    }

    public static void PurchaseOrderAdd(QBSessionManager sessMgr, IMsgSetRequest req, PurchaseOrder po) {
      Dictionary<string, string> balanceCatsMap = new Dictionary<string, string>() {
        { "STEEL", "Steel Inventory" },
        { "FLANGE", "Flange Inventory" },
        { "HETO", "HETO Inventory" },
        { "SNAPLOCK", "Snaplock Inventory" },
        { "JOB MATERIALS", "Job Materials" },
        { "WAREHOUSE", "Warehouse Item" },
        { "BUILDING MAINTENANCE", "Building Maintenance" },
        { "FREIGHT", "Freight Paid" },
        { "MACHINERY", "Machinery" },
        { "VENDOR CHARGES", "Vendor Charges" },
        { "TOOLS", "Tools" },
        { "MISC SHOP SUPPLIES", "Misc Shop Supplies" },
      };
      IPurchaseOrderAdd poaddreq = req.AppendPurchaseOrderAddRq();
      poaddreq.VendorRef.FullName.SetValue(po.Vendor.Name);
      poaddreq.IsToBePrinted.SetValue(false);
      poaddreq.IsToBeEmailed.SetValue(false);
      if (po.Memo != "") {
        poaddreq.Memo.SetValue(po.Memo);
      }
      po.Items.ForEach((poi) => {
        IORPurchaseOrderLineAdd poli = poaddreq.ORPurchaseOrderLineAddList.Append();
        // Other1 can be used as line item note for receiving template
        string temp;
        string name;
        if (balanceCatsMap.TryGetValue(poi.BalanceCategory.ToUpper(), out temp)) {
          name = temp;
        } else {
          name = poi.Name;
        }
        string description = poi.Description;
        bool hasMpn = poi.PartNumber != null && poi.PartNumber != "";
        if (hasMpn) {
          description = $"{poi.PartNumber} - {poi.Description}";
        }
        Console.WriteLine($"Set full name for QB: {name}.");
        poli.PurchaseOrderLineAdd.ItemRef.FullName.SetValue(name);
        poli.PurchaseOrderLineAdd.Desc.SetValue(description);
        poli.PurchaseOrderLineAdd.Quantity.SetValue(poi.Quantity);
        poli.PurchaseOrderLineAdd.Rate.SetValue(Math.Round(poi.PurchasePriceEach, 4));

        string mpn = hasMpn ? poi.PartNumber : poi.Name;
        if (mpn.Length > 31) {
          bool isTooLong = true;
          while (isTooLong) {
            mpn = UIHelper.UserInputLoop($"Given name of: {mpn} exceeds QB character limit. Please enter a shortended version (under 31 characters) for the manufacturer part number field.");
            if (mpn.Length <= 31) {
              isTooLong = false;
            }
          }
        }

        poli.PurchaseOrderLineAdd.ManufacturerPartNumber.SetValue(mpn);
        if (poi.Note != "") {
          poli.PurchaseOrderLineAdd.Other1.SetValue(poi.Note);
        }
      });

      Console.WriteLine("QB purchase order request built...");
      Console.WriteLine("Sending QB purchase order request...");

      IMsgSetResponse res = null;

      try {
        res = sessMgr.DoRequests(req);
      } catch (Exception e) {
        handleError(e, sessMgr);
      }

      Console.WriteLine("QB response received...");

      // for debugging
      Console.WriteLine(res.ToXMLString());
      return;
    }

    public static string AddSimpleInvoice(QBSessionManager sessMgr, IMsgSetRequest req, List<SimpleGroup> sgs) {
      string invoiceNumber = "";
      try {
        BuildSimpleInvoice(req, sgs);
        Console.WriteLine("QB simple invoice request built...");

        Console.WriteLine("Sending QB simple invoice request...");
        IMsgSetResponse res = sessMgr.DoRequests(req);
        Console.WriteLine("QB response received...");

        Console.WriteLine(res.ToXMLString());
        invoiceNumber = WalkInvoiceForInvoiceNumber(res);

        return invoiceNumber;
      } catch (Exception e) {
        handleError(e, sessMgr);
        return invoiceNumber;
      }
    }

    private static void BuildSimpleInvoice(IMsgSetRequest req, List<SimpleGroup> sgs) {
      IInvoiceAdd invReq = req.AppendInvoiceAddRq();
      invReq.CustomerRef.FullName.SetValue(sgs[0].Customer);
      invReq.IsToBePrinted.SetValue(false);
      if (sgs[0].Customer == "Cash Customer") {
        invReq.IsToBeEmailed.SetValue(false);
      } else {
        invReq.IsToBeEmailed.SetValue(true);
      }
      invReq.PONumber.SetValue(sgs[0].PO);
      invReq.Memo.SetValue($"Sales Order: {sgs[0].OrderNumber}");
      invReq.Other.SetValue(sgs[0].OrderNumber);
      AddSimpleInvoiceLineItems(invReq, sgs);
    }

    private static void AddSimpleInvoiceLineItems(IInvoiceAdd invReq, List<SimpleGroup> sgs) {
      foreach (var sg in sgs) {
        if (sg.Price > 0.00) {
          IORInvoiceLineAdd elem = invReq.ORInvoiceLineAddList.Append();
          elem.InvoiceLineAdd.Quantity.SetValue(sg.Quantity);
          elem.InvoiceLineAdd.ItemRef.FullName.SetValue(sg.Type);
          elem.InvoiceLineAdd.Desc.SetValue(sg.Name);
          elem.InvoiceLineAdd.ORRatePriceLevel.Rate.SetValue(Math.Round(sg.Price, 2));
        }
      }
    }

    public static string AddLineItemInvoice(QBSessionManager sessMgr, IMsgSetRequest req, List<LineItem> lis) {
      string invoiceNumber = "";
      try {
        BuildLineItemInvoice(req, lis);
        Console.WriteLine("QB line item invoice request built...");

        Console.WriteLine("Sending QB line item invoice request...");
        IMsgSetResponse res = sessMgr.DoRequests(req);
        Console.WriteLine("QB response received...");

        Console.WriteLine(res.ToXMLString());
        invoiceNumber = WalkInvoiceForInvoiceNumber(res);

        return invoiceNumber;
      } catch (Exception e) {
        handleError(e, sessMgr);
        return invoiceNumber;
      }
    }

    private static void BuildLineItemInvoice(IMsgSetRequest req, List<LineItem> lis) {
      IInvoiceAdd invReq = req.AppendInvoiceAddRq();
      invReq.CustomerRef.FullName.SetValue(lis[0].Customer);
      invReq.IsToBePrinted.SetValue(false);
      if (lis[0].Customer == "Cash Customer") {
        invReq.IsToBeEmailed.SetValue(false);
      } else {
        invReq.IsToBeEmailed.SetValue(true);
      }
      invReq.PONumber.SetValue(lis[0].PO);
      invReq.Memo.SetValue($"Sales Order: {lis[0].OrderNumber}");
      invReq.Other.SetValue(lis[0].OrderNumber);
      AddLineItemInvoiceLineItems(invReq, lis);
    }

    private static void AddLineItemInvoiceLineItems(IInvoiceAdd invReq, List<LineItem> lis) {
      // line items should be sanitized!
      foreach (var nli in lis) {
        var li = nli;
        IORInvoiceLineAdd elem = invReq.ORInvoiceLineAddList.Append();
        elem.InvoiceLineAdd.Quantity.SetValue(li.Quantity);
        if (li.Category == "Stock Pull" && li.Name.StartsWith("J PIPE")) {
          li = nli with { Category = "Round Duct" };
        }
        if (li.Category == "Stock Pull" && li.Name.StartsWith("BOXG10")) {
          li = nli with { Category = "Misc Rectangular" };
        }
        if (li.Category == "Stock Pull" && li.Name.StartsWith("SLEEVE - TUBE")) {
          li = nli with { Category = "Misc Round Oval" };
        }

        string temp;
        string cat;
        if (catsMap.TryGetValue(li.Category, out temp)) {
          cat = temp;
        } else {
          cat = li.Category;
        }

        elem.InvoiceLineAdd.ItemRef.FullName.SetValue(cat);
        if (li.Name != "") {
          elem.InvoiceLineAdd.Desc.SetValue(li.Name);
        }
        elem.InvoiceLineAdd.ORRatePriceLevel.Rate.SetValue(Math.Round(li.PriceEach, 2));
      }
    }

    private static string WalkInvoiceForInvoiceNumber(IMsgSetResponse res) {
      Console.WriteLine("Walking QB response for new invoice number...");
      string inv = "";
      if (res == null) {
        return inv;
      }
      IResponseList resList = res.ResponseList;
      if (resList == null) {
        return inv;
      }
      IResponse r = resList.GetAt(0); // only one req sent
      if (r.StatusCode >= 0) {
        if (r.Detail != null) {
          ENResponseType resType = (ENResponseType)r.Type.GetValue();
          if (resType == ENResponseType.rtInvoiceAddRs) {
            IInvoiceRet ir = (IInvoiceRet)r.Detail;
            if (ir != null) {
              if (ir.RefNumber != null) {
                inv = (string)ir.RefNumber.GetValue();
              }
            }
          }
        }
      }
      return inv;
    }
  }
}
