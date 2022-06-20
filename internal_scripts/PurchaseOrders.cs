using System;
using System.IO;
using System.Threading.Tasks;

using Interop.QBFC14;

namespace master {
  public class PurchaseOrders {
    public static async Task Run() {
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
        UIHelper.SectionDelimiter();

        string poId = "";
        bool isPOIdSet = false;
        while (!isPOIdSet) {
          poId = UIHelper.UserInputLoop("Please enter the desired purchase order number to write to QB:");
          if (poId == "") {
            Console.WriteLine("Invalid input provided. Please enter a valid purchase oder number.");
          } else {
            isPOIdSet = true;
          }
        }

        string url = $"{Types.baseURI}/purchase-orders/number/{poId}";
        PurchaseOrder po = await Client.GetPurchaseOrder(url);
        if (po == null) {
          Console.WriteLine("Failed to retrieve purchase order. Check for errors.");
          UIHelper.UserInputLoop("Press enter to close script.");
          return;
        }

        try {
          UIHelper.SectionDelimiter();
          Console.WriteLine("Starting session and opening QB connection...");
          sessMgr = new QBSessionManager();
          var req = QBCommon.CreateQBReq(sessMgr);
          connOpen = QBCommon.QBConnect(sessMgr);
          sessionBegun = QBCommon.QBSession(sessMgr);
          Console.WriteLine("QB connection successful...");
          QBCommon.PurchaseOrderAdd(sessMgr, req, po);

          UIHelper.SectionDelimiter();
          Console.WriteLine("Purchase order written to QB. Please verify correctness and accuracy.");
          Cleanup();
          UIHelper.UserInputLoop("Press enter to close script.");
          return;
        } catch (Exception e) {
          Console.WriteLine($"Error encountered: {e}.");
          Cleanup();
          UIHelper.UserInputLoop("Press enter to close script.");
          return;
        }
      } catch (Exception e) {
        Console.WriteLine($"Error encountered: {e}.");
        UIHelper.UserInputLoop("Press enter to close script.");
        return;
      }
    }
  }
}
