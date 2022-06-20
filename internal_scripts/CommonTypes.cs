/*
 * public record for Billing record types (simple and line item)
 */

using System;
using System.Collections.Generic;
using System.Linq;

namespace master {
  public class Types {
    public static string baseURI = "https://erp.com";
    // public static string baseURI = "http://localhost:4000";
  }
  // consider adding additional labor mins and additional fixed cost fields
  public record LineItem(string Customer,
                         string OrderNumber,
                         string PO,
                         string Category,
                         double Quantity,
                         string Name,
                         string Gauge,
                         string Material,
                         double MaterialWeight,
                         double Weight,
                         string LinerMaterial,
                         double LinerArea,
                         string SkinGauge,
                         string SkinMaterial,
                         double SkinWeight,
                         double AccessoryCost,
                         string Rate,
                         bool IsWrapped,
                         double PriceEach,
                         double Length, // only for straight duct/pipe for project tracking purposes
                         bool isCatalogue,
                         double? Labor);

  public record SimpleGroup(string OrderNumber,
                            string Customer,
                            string PO,
                            string Type,
                            string Name,
                            int cfiLBS,
                            double Price,
                            double Quantity);

  public record Section(int Id, string Name);

  public record CustomerFull(int Id, string Name, string Company, string Type);

  public record Project(int Id,
                        int AppId,
                        string Name,
                        CustomerFull Customer,
                        List<Section> Sections);

  public record CustomerRecord(int Id,
                               string name);

  public record PricingList(int Id,
                            string Description,
                            CustomerRecord Customer);

  public record Filtered(List<LineItem> Extras, Double NonExtrasTotal, Double NonExtrasWeight);

  public class Vendor {
    public string Name { get; set; }
  }

  public class PurchaseOrder {
    public Vendor Vendor { get; set; }
    public string Memo { get; set; }
    public int PO { get; set; }
    public List<POItem> Items { get; set; }
  }

  public class POItem {
    public string Name { get; set; }
    public string Description { get; set; }
    public string ReferenceType { get; set; }
    public string BalanceCategory { get; set; }
    public double Quantity { get; set; }
    public double PurchasePriceEach { get; set; }
    public string Note { get; set; } // Other1 field in QB
#nullable enable
    public string? PartNumber { get; set; }
#nullable disable
  }

  public class APIError {
    public string status { get; }
    public string msg { get; }
  }
}
