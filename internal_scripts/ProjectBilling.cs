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
  public class ProjectBilling {
    public static async Task Bill() {
      bool sessionBegun = false;
      bool connOpen = false;
      QBSessionManager sessMgr = null;

      void Cleanup() {
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

      try {
        string projectId = "";
        bool isProjectIdSet = false;
        while (!isProjectIdSet) {
          projectId = UIHelper.UserInputLoop("Please enter the desired project app id:");
          if (projectId == "") {
            Console.WriteLine("Invalid input provided. Please enter a valid app number.");
          } else {
            isProjectIdSet = true;
          }
        }

        string url = $"{Types.baseURI}/projects/appId/{projectId}/billing";
        var f = await Client.DoProjectBilling(url);
        if (f == null) {
          Console.WriteLine("Failed to return project billing results. Check for errors.");
          return;
        }

        var prj = await Client.RetrieveProjectData(projectId);

        UIHelper.SectionDelimiter();
        Console.WriteLine("Working on extras...");
        string extraOrderNumber = "";
        bool isExtraONSet = false;
        if (f.Extras.Count > 0) {
          Console.WriteLine("The following are extras:");
          f.Extras.ForEach((li) => {
            Console.WriteLine($"Extra: {li.Name} \t Quant: {li.Quantity} \t PriceEach: ${li.PriceEach}");
          });
          while (!isExtraONSet) {
            extraOrderNumber = UIHelper.UserInputLoop("Please input the desired sales order number for the extras:");
            if (extraOrderNumber == "") {
              Console.WriteLine("Please input a valid order number.");
            } else {
              isExtraONSet = true;
            }
          }

          var fixedExtras = f
            .Extras
            .Select(li => li with { OrderNumber = extraOrderNumber
                                  , Customer = prj.Customer.Name
                                  , Name = $"{li.Name} ({li.OrderNumber})"
                                  })
            .ToList();

          try {
            Console.WriteLine("Creating the sales order for extras.");

            Console.WriteLine("Starting session and opening QB connection...");
            sessMgr = new QBSessionManager();
            var req = QBCommon.CreateQBReq(sessMgr);
            connOpen = QBCommon.QBConnect(sessMgr);
            sessionBegun = QBCommon.QBSession(sessMgr);
            Console.WriteLine("QB connection successful...");
            UIHelper.SectionDelimiter();

            QBCommon.AddLineItemSalesOrder(sessMgr, req, fixedExtras);

            Console.WriteLine("Extras sales order written. Please verify and retrieve invoice number if applicable.");
          } catch (Exception e) {
            Console.WriteLine($"Exception encountered: {e}.");
            Cleanup();
          }

          UIHelper.SectionDelimiter();
        }

        Console.ForegroundColor = ConsoleColor.Red;
        Console.WriteLine("For non-extras:");
        Console.WriteLine($"Project billing total: ${f.NonExtrasTotal}.");
        Console.WriteLine($"Project billing total: {f.NonExtrasWeight} pounds.");
        Console.ResetColor();
        UIHelper.SectionDelimiter();

        Console.WriteLine("Verify correctness of numbers. Record tracking info. Record invoice number. Close billing cycle in ERP once complete.");
        return;
      } catch (Exception e) {
        Console.Write($"Error encountered: {e}.");
        return;
      }
    }
  }
}
