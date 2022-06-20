using System;
using System.IO;
using System.Security.Permissions;
using System.Collections.Generic;
using System.Linq;
using System.Globalization;
using System.Net.Http;
using System.Net.Http.Json;
using System.Net.Http.Headers;
using System.Text;
using System.Text.Json;
using System.Threading.Tasks;

using Interop.QBFC14;

namespace master {
  public class Program {
    static bool hasTriggered = false;
    static string projectAppId = "";
    static bool isProjectAppIdSet = false;
    static string projectSectionIdString = "";
    static bool isProjectSectionIdSet = false;
    static Project prj;
    static bool hasRetrievedProjectData = false;
    static string processType = "";
    static QBSessionManager sessMgr = null;
    static bool connOpen = false;
    static bool sessionBegun = false;
    static bool isRepeat = false;
    static bool stillRunWatcher = false;
    static bool isCloseToggle = false;

    public static async Task Main() {
      await RunStartup();
    }

    private static async Task RunStartup() {
      try {
        promptProcessType();
        UIHelper.SectionDelimiter();

        switch (processType) {
          case "Assemblies/BD - write":
            await handleBDWrite();
            break;
          case "Assemblies/BD - bill":
            await handleBDBill();
            break;
          case "billing":
            await handleBilling(); // ERP entry sans QB
            break;
          case "quote":
            await handleQuoting(); // TODO at some point no QB write!
            break;
          case "stock":
            await handleStockOrder(); // TODO improve to "smartify" system? (e.g. non-stock items mapping)
            break;
          case "project":
            await handleProjectOrder();
            break;
          case "project - bill":
            await handleProjectBilling();
            break;
          case "purchase orders":
            await handlePurchaseOrders();
            break;
          case "invoice":
            await handleInvoicing();
            break;
          case "sales order":
            await handleSalesOrder();
            break;
          case "exit":
            Console.WriteLine("Closing script...");
            isCloseToggle = true;
            break;
        }
        if (isCloseToggle) {
          Environment.Exit(0);
          return;
        } else {
          string isContinue = UIHelper.UserInputLoop("Continue with same process? Enter 'Y/y/yes' to continue. NOTE: if changing projects, do NOT select yes.");
          if (isContinue == "y" || isContinue == "Y" || isContinue == "yes") {
            isRepeat = true;
            isProjectSectionIdSet = false; // permit changing project section per order
            projectSectionIdString = "";
          } else {
            isRepeat = false;
            projectAppId = "";
            isProjectAppIdSet = false;
            projectSectionIdString = "";
            isProjectSectionIdSet = false;
            prj = null;
            hasRetrievedProjectData = false;
          }
          await RunStartup();
          return;
        }
      } catch (Exception e) {
        Console.WriteLine($"Top-level error caught: {e}.");
        return;
      }
    }

    private static Task handleQuoting() {
      string path = "C:/AUTODESK TO QUICKBOOKS/QUOTE/";
      var t = Run(path, quoter);
      try {
        t.Start();
        return t;
      } catch (Exception e) {
        Console.WriteLine($"Encountered exception: {e}.");
        return t;
      }
    }

    private static Func<List<LineItem>, Task<bool>> quoter = async (clis) => {
      try {
        clis = promptWrap(clis);
        string url = Types.baseURI + "/quotes/QL";
        bool isPricingList = promptPricingList();
        if (isPricingList) {
          url = await retrievePricingList(url);
        }
        var pricedLis = await Client.RunPricing(url, clis);
        if (pricedLis != null) {
          UIHelper.SectionDelimiter();
        } else {
          Console.WriteLine("Failed to retrieve pricing from the ERP. Check for error messages and/or a duplicate quote/order number (they must be unique).");
          UIHelper.SectionDelimiter();
          hasTriggered = false;
          return false;
        }

        var req = openQB();

        string setType = promptType();
        if (setType == "simple") {
          Console.WriteLine("Building QB Simple SO/quote request...");
          var simpleGroups = Simple.Aggregate(pricedLis);
          simpleGroups.ForEach(Console.WriteLine);
          QBCommon.AddSimpleQuote(sessMgr, req, simpleGroups);
          closeQB();
          return true;
        } else {
          QBCommon.AddLineItemQuote(sessMgr, req, pricedLis);
          closeQB();
          return true;
        }
      } catch (Exception e) {
        Console.WriteLine($"Encountered exception: {e}.");
        Cleanup();
        return false;
      }
    };

    private static Task handleBilling() {
      string path = "C:/AUTODESK TO QUICKBOOKS/SALES ORDER/";
      var t = Run(path, biller);
      try {
        t.Start();
        return t;
      } catch (Exception e) {
        Console.WriteLine($"Encountered exception: {e}.");
        return t;
      }
    }

    private static Func<List<LineItem>, Task<bool>> biller = async (clis) => {
      try {
        clis = promptWrap(clis);
        string url = Types.baseURI + "/orders/FS";
        bool isPricingList = promptPricingList();
        if (isPricingList) {
          url = await retrievePricingList(url);
        }
        var pricedLis = await Client.RunPricing(url, clis);
        if (pricedLis != null) {
          UIHelper.SectionDelimiter();
          return true;
        } else {
          Console.WriteLine("Failed to retrieve pricing from the ERP. Check for error messages and/or a duplicate quote/order number (they must be unique).");
          UIHelper.SectionDelimiter();
          hasTriggered = false;
          return false;
        }
      } catch (Exception e) {
        Console.WriteLine($"Encountered exception: {e}.");
        return false;
      }
    };

    private static async Task handleInvoicing() {
      try {
        string orderNumber = promptOrderNumber();
        string invoiceNumber = await HandleFabricatedList(orderNumber, true);
        await writeInvoiceNumbers(orderNumber, invoiceNumber, true);
        return;
      } catch (Exception e) {
        Console.WriteLine($"Encountered exception: {e}.");
      }
    }

    private static async Task handleSalesOrder() {
      try {
        string orderNumber = promptOrderNumber();
        await HandleSalesOrderWrite(orderNumber);
        return;
      } catch (Exception e) {
        Console.WriteLine($"Encountered exception: {e}.");
      }
    }

    private static async Task HandleSalesOrderWrite(string orderNumber) {
      try {
        var lis = await Client.GetFabricatedListItems(orderNumber);
        if (lis == null) {
          Console.WriteLine("Failure to retrieve. Closing script.");
          return;
        }

        var req = openQB();

        string setType = promptType();
        if (setType == "simple") {
          List<SimpleGroup> simpleGroups = new List<SimpleGroup>();
          Console.WriteLine("Building simple billing groups...");
          simpleGroups = Simple.Aggregate(lis);
          simpleGroups.ForEach(Console.WriteLine);
          QBCommon.AddSimpleSalesOrder(sessMgr, req, simpleGroups);
        } else {
          QBCommon.AddLineItemSalesOrder(sessMgr, req, lis);
        }

        closeQB();
        return;
      } catch (Exception e) {
        Console.WriteLine($"Encountered exception: {e}.");
      }
    }

    private static async Task<string> HandleFabricatedList(string orderNumber, bool checkSetType) {
      string invoiceNumber = "";
      try {
        var lis = await Client.GetFabricatedListItems(orderNumber);
        if (lis == null) {
          Console.WriteLine("Failure to retrieve. Closing script.");
          return "";
        }

        var req = openQB();

        string setType = "line";
        if (checkSetType) {
          setType = promptType();
        } else {
          // default
          setType = "line";
        }
        if (setType == "simple") {
          List<SimpleGroup> simpleGroups = new List<SimpleGroup>();
          Console.WriteLine("Building simple billing groups...");
          simpleGroups = Simple.Aggregate(lis);
          simpleGroups.ForEach(Console.WriteLine);
          invoiceNumber = QBCommon.AddSimpleInvoice(sessMgr, req, simpleGroups);
        } else {
          invoiceNumber = QBCommon.AddLineItemInvoice(sessMgr, req, lis);
        }

        closeQB();
        return invoiceNumber;
      } catch (Exception e) {
        Console.WriteLine($"Encountered exception: {e}.");
        return invoiceNumber;
      }
    }

    private static Task handleProjectOrder() {
      string path = "C:/AUTODESK TO QUICKBOOKS/SALES ORDER/";
      var t = Run(path, projectBiller);
      try {
        t.Start();
        return t;
      } catch (Exception e) {
        Console.WriteLine($"Encountered exception: {e}.");
        Cleanup();
        return t;
      }
    }

    private static Func<List<LineItem>, Task<bool>> projectBiller = async (clis) => {
      try {
        while (!isProjectAppIdSet) {
          projectAppId = UIHelper.UserInputLoop("Lookup and input the correct project app id number (NOT NAME):");
          if (projectAppId != "") {
            isProjectAppIdSet = true;
          } else {
            Console.WriteLine("Please input a project app id number.");
          }
        }

        string url = Types.baseURI;
        if (!hasRetrievedProjectData) {
          prj = await Client.RetrieveProjectData(projectAppId);
          if (prj != null) {
            hasRetrievedProjectData = true;
          } else {
            Console.WriteLine("Failed to retrieve project data. Check log for errors.");
            hasRetrievedProjectData = false;
            return false;
          }
        }

        Console.WriteLine("Select the section number to which to assign the order:");
        while (!isProjectSectionIdSet) {
          for (int i = 0; i < prj.Sections.Count; i++) {
            Console.WriteLine($"({i}): {prj.Sections[i].Name}");
          }
          projectSectionIdString = UIHelper.UserInputLoop("Enter the correct section number:");

          int projectSectionId;
          try {
            projectSectionId = int.Parse(projectSectionIdString);
          } catch (Exception e) {
            Console.WriteLine($"Failed to parse input: ${e}. Try again.");
            projectSectionId = -1; // ensures failure of next check
          }

          if (projectSectionId >=0 && projectSectionId < prj.Sections.Count) {
            int chosenSection = prj.Sections[projectSectionId].Id;
            Console.WriteLine($"The order will be written to the section: {prj.Sections[projectSectionId].Name}.");
            isProjectSectionIdSet = true;
            url += $"/projects/appId/{projectAppId}/sections/{chosenSection}/orders";
          } else {
            Console.WriteLine("Please try again.");
          }
        }

        Console.WriteLine("WARNING: Choose NO LDS if they were covered by the project PO, regardless of whether or not the order has LDS! Only choose LDS if they are NOT covered under the PO and the order has them.");
        clis = promptWrap(clis);

        var pricedLis = await Client.RunPricing(url, clis);
        if (pricedLis != null) {
          UIHelper.SectionDelimiter();
          Console.WriteLine("Project order written to ERP. Verify correctness.");
          hasTriggered = false;
          return true;
        } else {
          Console.WriteLine("Failed to retrieve pricing from the ERP. Check for error messages and/or a duplicate quote/order number (they must be unique).");
          UIHelper.SectionDelimiter();
          hasTriggered = false;
          return false;
        }
      } catch (Exception e) {
        Console.WriteLine($"Encountered exception: {e}.");
        return false;
      }
    };

    private static async Task handleProjectBilling() {
      try {
        await ProjectBilling.Bill();
      } catch (Exception e) {
        Console.WriteLine($"Encountered exception: {e}.");
        Cleanup();
      }
    }

    private static async Task handlePurchaseOrders() {
      try {
        await PurchaseOrders.Run();
      } catch (Exception e) {
        Console.WriteLine($"Encountered exception: {e}.");
        Cleanup();
      }
    }

    private static Task handleBDWrite() {
      string path = "C:/AUTODESK TO QUICKBOOKS/SALES ORDER/";
      var t = Run(path, bdwriter);
      try {
        t.Start();
        return t;
      } catch (Exception e) {
        Console.WriteLine($"Encountered exception: {e}.");
        return t;
      }
    }

    private static Func<List<LineItem>, Task<bool>> bdwriter = async (clis) => {
      try {
        var isSuccess = await Client.PostBDOrder(clis);
        if (isSuccess == 0) {
          Console.WriteLine("Error occurred during order write.");
          return false;
        }
        return true;
      } catch (Exception e) {
        Console.WriteLine($"Encountered exception: {e}.");
        Cleanup();
        return false;
      }
    };

    private static async Task handleBDBill() {
      try {
        string orderNumber = promptOrderNumber();
        string invoiceNumber = await HandleFabricatedList(orderNumber, false);
        await writeInvoiceNumbers(orderNumber, invoiceNumber, false);
        return;
      } catch (Exception e) {
        Console.WriteLine($"Encountered exception: {e}.");
        Cleanup();
        return;
      }
    }

    private static Task handleStockOrder() {
      string path = "C:/AUTODESK TO QUICKBOOKS/SALES ORDER/";
      var t = Run(path, stockwriter);
      try {
        t.Start();
        return t;
      } catch (Exception e) {
        Console.WriteLine($"Encountered exception: {e}.");
        return t;
      }
    }

    private static Func<List<LineItem>, Task<bool>> stockwriter = async (clis) => {
      try {
        string url = Types.baseURI + "/stock/order";
        var pricedLis = await Client.RunPricing(url, clis);
        if (pricedLis != null) {
          UIHelper.SectionDelimiter();
          Console.WriteLine("Stock order written to ERP. Verify results.");
          return true;
        }
        Console.WriteLine("Stock order write failed. Check log for error(s) and/or duplicate order number (must be unique).");
        return false;
      } catch (Exception e) {
        Console.WriteLine($"Encountered exception: {e}.");
        Cleanup();
        return false;
      }
    };

    private static List<LineItem> promptWrap(List<LineItem> lis) {
      string wrapString = UIHelper.UserInputLoop("Is the job wrapped? If so, enter 'Y/y/yes' to apply the straight, X% markup (defaults to no):");
      bool isWrapped = wrapString == "y" || wrapString == "Y" || wrapString == "yes";
      return lis
        .Select(li => {
          return li with { IsWrapped = isWrapped };
        })
        .ToList();
    }

    private static bool promptPricingList() {
      string isCatalogString = UIHelper.UserInputLoop("Is the pricing determined via a pricing catalog? If yes, enter 'Y/y/yes', otherwise press enter to continue.");
      return isCatalogString == "Y" || isCatalogString == "y" || isCatalogString == "yes";
    }

    private static string promptType() {
      bool isTypeSet = false;
      string setType = "";
      List<string> types = new List<string>() { "simple", "line" };
      while (!isTypeSet) {
        for (int i = 0; i < types.Count; i++) {
          Console.WriteLine($"({i + 1}): {types[i]}");
        }
        string typeIdxString = UIHelper.UserInputLoop("Enter the desired billing type number:");
        int typeIdx;
        try {
          typeIdx = int.Parse(typeIdxString);
        } catch (Exception e) {
          Console.WriteLine($"Failed to parse input: ${e}. Try again.");
          typeIdx = -1; // ensures failure of next check
        }

        if (typeIdx - 1 >= 0 && typeIdx - 1 < types.Count) {
          setType = types[typeIdx - 1];
          Console.WriteLine($"The chosen billing type is: {setType}.");
          isTypeSet = true;
        } else {
          isTypeSet = false;
        }
      }
      return setType;
    }

    public static void Cleanup() {
      hasTriggered = false;
      if (sessionBegun) {
        sessMgr.EndSession();
      }
      sessionBegun = false;
      if (connOpen) {
        sessMgr.CloseConnection();
      }
      connOpen = false;
      return;
    }

    private static Task Run(string path, Func<List<LineItem>, Task<bool>> cb) {
      stillRunWatcher = true;
      Task t = new Task(() => {
        Console.WriteLine("Listening for file changes...");
        using (FileSystemWatcher watcher = new FileSystemWatcher()) {
          watcher.Path = path;
          watcher.NotifyFilter = NotifyFilters.LastWrite;
          watcher.Filter = "*.csv";
          watcher.Changed += OnChanged(cb);
          watcher.EnableRaisingEvents = true;
          while (stillRunWatcher) ;
        }
      });
      return t;
    }

    private static System.IO.FileSystemEventHandler OnChanged(Func<List<LineItem>, Task<bool>> cb) {
      return (object source, FileSystemEventArgs e) => {
        try {
          if (hasTriggered) {
            return;
          }
          Console.WriteLine("File change detected: starting process...");
          hasTriggered = true;
          Task.Delay(3000)
            .ContinueWith(async t => {
              UIHelper.SectionDelimiter();

              List<LineItem> clis = new List<LineItem>();
              Console.WriteLine("Reading from csv...");
              try {
                clis = Cleaner.CleanCamDuctCSV(e.FullPath);
                bool isDone = await cb(clis);
                hasTriggered = false;
                stillRunWatcher = false;
                return isDone;
              } catch (Exception e) {
                Console.WriteLine($"Exception caught when reading/parsing csv file:\n{e}");
                Cleanup();
                hasTriggered = false;
                stillRunWatcher = false;
                return false;
              }
          });
        } catch (Exception err) {
          Console.WriteLine($"Encountered exception: {err}.");
          Cleanup();
          hasTriggered = false;
          stillRunWatcher = false;
          return;
        }
      };
    }

    private static async Task<string> retrievePricingList(string url) {
      var pricingLists = await Client.GetPricingLists();
      bool isListSelected = false;
      Console.WriteLine("Select the desired pricing list number:");
      while (!isListSelected) {
        for (int i = 0; i < pricingLists.Count; i++) {
          Console.WriteLine($"({i}): {pricingLists[i]}");
        }
        string catIdString = UIHelper.UserInputLoop("Enter the desired pricing list number:");

        int catId;
        try {
          catId = int.Parse(catIdString);
        } catch (Exception e) {
          Console.WriteLine($"Failed to parse input: ${e}. Try again.");
          catId = -1; // ensures failure of check
        }

        if (catId >=0 && catId < pricingLists.Count) {
          int chosenCat = pricingLists[catId].Id;
          Console.WriteLine($"The order will be priced via the pricing list, {pricingLists[catId].Description} where applicable.");
          isListSelected = true;
          url += $"/catalog/{chosenCat}";
        } else {
          Console.WriteLine("Please try again.");
        }
      }
      return url;
    }

    private static IMsgSetRequest openQB() {
      Console.WriteLine("Starting session and opening QB connection...");
      sessMgr = new QBSessionManager();
      var req = QBCommon.CreateQBReq(sessMgr);
      connOpen = QBCommon.QBConnect(sessMgr);
      sessionBegun = QBCommon.QBSession(sessMgr);
      Console.WriteLine("QB connection successful...");
      UIHelper.SectionDelimiter();
      return req;
    }

    private static void closeQB() {
      Cleanup();
      Console.WriteLine("QB connection closed.");
      hasTriggered = false;
      Console.WriteLine("Received response from QB without error. VERIFY SO/QUOTE ACCURACY!");
      UIHelper.SectionDelimiter();
      return;
    }

    private static string promptOrderNumber() {
      bool isSet = false;
      string orderNumber = "";
      while (!isSet) {
        orderNumber = UIHelper.UserInputLoop("Enter the desired order number:");
        if (orderNumber == "") {
          Console.WriteLine($"Invalid order number entered: {orderNumber}. Try again.");
        } else {
          isSet = true;
        }
      }
      return orderNumber;
    }

    private static void promptProcessType() {
      if (!isRepeat) {
        UIHelper.SectionDelimiter();
        Console.WriteLine("Starting billing, quotes, stock orders, and pricing script...");

        bool isProcessTypeSet = false;

        List<string> processes = new List<string>(){"billing", "quote", "invoice", "sales order", "stock", "project", "project - bill", "Assemblies/BD - write", "Assemblies/BD - bill", "purchase orders", "exit"};
        Console.WriteLine("Select the process type number to run:");
        while (!isProcessTypeSet) {
          for (int i = 0; i < processes.Count; i++) {
            Console.WriteLine($"({i}): {processes[i]}");
          }
          string processTypeIdxString = UIHelper.UserInputLoop("Enter the desired process number:");
          int processTypeIdx;
          try {
            processTypeIdx = int.Parse(processTypeIdxString);
          } catch (Exception e) {
            Console.WriteLine($"Failed to parse input: ${e}. Try again.");
            processTypeIdx = -1; // ensures failure of next check
          }

          if (processTypeIdx >= 0 && processTypeIdx < processes.Count) {
            processType = processes[processTypeIdx];
            Console.WriteLine($"The chosen process is: {processType}.");
            isProcessTypeSet = true;
          } else {
            isProcessTypeSet = false;
          }
        }
      }
    }

    private static async Task writeInvoiceNumbers(string orderNumber, string invoiceNumber, bool writeToERP) {
      UIHelper.SectionDelimiter();
      Console.WriteLine($"New invoice number is {invoiceNumber}.");
      try {
        Console.WriteLine("Writing invoice number to old App...");
        var res1 = await Client.MarkInvoiceNumberOLD(invoiceNumber, orderNumber);
        if (res1 != 0) {
          Console.WriteLine($"Failed to write invoice number to old App. Please write invoice number {invoiceNumber} to order {orderNumber} by hand and report error to admin.");
        }
      } catch (Exception ei1) {
        Console.WriteLine($"Encountered exception: {ei1}.");
        Console.WriteLine($"Failed to write invoice number to old App. Please write invoice number {invoiceNumber} to order {orderNumber} by hand and report error to admin.");
      }

      // BD orders need additional step to ensure built assemblies are subtracted from inventory. This is done via button on their sub-order page.
      // Copy and paste invoice number as prompted.
      if (writeToERP) {
        try {
          Console.WriteLine("Writing invoice number to ERP and marking order as billed...");
          var res2 = await Client.MarkInvoiceNumber(invoiceNumber, orderNumber);
          if (res2 != 0) {
            Console.WriteLine($"Failed to write invoice number to ERP. Please contact admin with any error message(s).");
          }
        } catch (Exception ei2) {
          Console.WriteLine($"Encountered exception: {ei2}.");
        }
      } else {
        Console.WriteLine($"IMPORTANT: please copy and paste the invoice number ${invoiceNumber} in the prompt that comes up when marking the BD order as billed in the ERP.");
      }
      return;
    }
  }
}
