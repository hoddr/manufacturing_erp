using System;
using System.IO;
using System.Globalization;
using System.Collections.Generic;
using System.Linq;

using CsvHelper;

namespace master {
  public class Cleaner {
    private record RawCamDuctCSVLine(string Customer,
                                     string JobName,
                                     string SalesOrder,
                                     string PO,
                                     string CustomerContact,
                                     string ShipDate,
                                     string CID,
                                     string Type,
                                     string TAG,
                                     string CostCode,
                                     string Name,
                                     string Description,
                                     double Quantity,
                                     string Dims1,
                                     string Dims2,
                                     string Dims3,
                                     string Dims4,
                                     string Dims5,
                                     string Dims6,
                                     string Dims7,
                                     string Dims8,
                                     string Dims9,
                                     string Dims10,
                                     string Angle,
                                     string Connector1,
                                     string Connector2,
                                     string Connector3,
                                     string Connector4,
                                     string Gauge,
                                     string Material,
                                     string Material2,
                                     string MainFacing,
                                     string Notes,
                                     double PriceEach,
                                     string CustomFieldDoNotBill, // unused
                                     double Weight, // lbs (total including liner, skin, accessories)
                                     string Rate, // mins for camduct
                                     string SkinGauge, // double wall
                                     string SkinMaterial, // double wall
                                     double LinerArea, // insulation/liner
                                     double MaterialWeight, // no-skin
                                     string SkinWeight,  // double wall - will be blank on some so double won't work
                                     double VaneMatCost,
                                     double ConnectorMatCost,
                                     double StiffenerMatCost);

    private record DimOptionLine(string Description,
                                 string Dims1,
                                 string Dims2,
                                 string Dims3,
                                 string Dims4,
                                 string Dims5,
                                 string Dims6,
                                 string Dims7,
                                 string Dims8,
                                 string Dims9,
                                 string Dims10);

    private record ConnRename(string FullName, string AbbvName);

    private record Fitting(string Name, string ProductGroup);

    public static List<LineItem> CleanCamDuctCSV(string file) {
      var rawLines = ReadRawCamDuctCSV(file);
      var cleanLines = CleanRawCamDuctCSV(rawLines);
      return cleanLines;
    }

    private static List<LineItem> CleanRawCamDuctCSV(List<RawCamDuctCSVLine> rls) {
      Dictionary<string, string> purchasesMap = new Dictionary<string, string>() {
        { "DP 1010 GALLON", "DP1010G" },
        { "DP 1010 TUBE", "DP1010T" },
        { "DP 1010 SPRAY", "DP1010S" },
        { "SOLVENT DP 1090 GALLON", "DP1090G" },
        { "SOLVENT DP 1090 TUBE", "DP1090T" },
        { "HANGER 9 IN", "HANGER 9 INCH" },
        { "HANGER 11 IN", "HANGER 11 INCH" },
        { "36 INCH ROLL BLUE PLASTIC", "DD:PRW36-200" },
        { "24 INCH ROLL BLUE PLASTIC", "DD:PRW24-200" },
        { "48 INCH ROLL BLUE PLASTIC", "DD:PRW48-200" },
        { "12 INCH ROLL BLUE PLASTIC", "PRW12-200" },
        { "30 INCH ROLL BLUE PLASTIC", "PRW30-200" },
        { "DM Corner - Galv", "DD:Corner-DM-Galv" },
        { "DM Corner - Alum", "DD:Corner-DM-Alum" },
        { "DM Corner - Stainless", "DD:Corner-DM-Stainless" },
        { "1IN BRUSH", "DD:BB1" },
        { "1.5IN BRUSH", "DD:BB15" },
      };
      List<LineItem> cls = new();
      List<DimOptionLine> dimopts = ReadDimsCSV();
      string masterPO = null;
      foreach (var r in rls) {
        string customer = r.Customer == "" ? "-" : r.Customer;
        string type = r.Type;
        string description = "";
        // tweak columns for dimensions, connectors, angle, name, material
        if (r.CostCode == "") {
          List<string> dimLists = new List<string>();

          foreach (var d in dimopts) {
            if (d.Description == r.Description) {
              dimLists.Add(d.Dims1 == "" ? "" : r.Dims1);
              dimLists.Add(d.Dims2 == "" ? "" : r.Dims2);
              dimLists.Add(d.Dims3 == "" ? "" : r.Dims3);
              dimLists.Add(d.Dims4 == "" ? "" : r.Dims4);
              dimLists.Add(d.Dims5 == "" ? "" : r.Dims5);
              dimLists.Add(d.Dims6 == "" ? "" : r.Dims6);
              dimLists.Add(d.Dims7 == "" ? "" : r.Dims7);
              dimLists.Add(d.Dims8 == "" ? "" : r.Dims8);
              dimLists.Add(d.Dims9 == "" ? "" : r.Dims9);
              dimLists.Add(d.Dims10 == "" ? "" : r.Dims10);
            }
          }

          // if didn't match any descriptions, keep original raw output
          if (dimLists.Count == 0) {
            dimLists.Add(r.Dims1);
            dimLists.Add(r.Dims2);
            dimLists.Add(r.Dims3);
            dimLists.Add(r.Dims4);
            dimLists.Add(r.Dims5);
            dimLists.Add(r.Dims6);
            dimLists.Add(r.Dims7);
            dimLists.Add(r.Dims8);
            dimLists.Add(r.Dims9);
            dimLists.Add(r.Dims10);
          }

          description += r.Description + " ";
          var filteredDims = dimLists.Where(d => d != "").ToList();
          foreach (var d in filteredDims) {
            description += d + "x";
          }

          description = description.Remove(description.Length - 1); // need to remove last "x"

          if (r.Angle != "") {
            description += " " + r.Angle + " deg";
          }

          var conns = SanitizeConnectors(r);
          if (conns[0] != "") {
            description += " " + conns[0];
            var filteredConns = conns.Where(c => c != "").ToList();
            for (int i = 1; i < filteredConns.Count; i++) {
              description += " to " + filteredConns[i];
            }
          }

          if (r.Gauge != "") {
            description += " " + r.Gauge;
          }

          if (r.Material != "") {
            description += " " + r.Material;
          }

          if (r.Material2 != "") {
            description += " " + r.Material2;
          }

          if (r.Notes != "") {
            // note that the semi-colon is stripped for security purposes - note i just tacked on
            description += "; " + r.Notes;
          }
        } else {
          type = r.CostCode;
          description = "";
        }

        if (r.Type == "Warm Air Stock") {
          description = "SNAPLOCK:" + r.Description;
        }
        // only add prefix for non j or non spiral duct liner
        if (r.Type == "Stock Pull" && !(r.Name.StartsWith("J PIPE") || r.Name.StartsWith("SDL1") || r.Name.StartsWith("BOXG10") || r.Name.StartsWith("Bel"))) {
          description = "STOCK SPIRAL:" + r.Description;
        }

        if (r.Type == "Round Duct" && r.Name.StartsWith("J PIPE")) {
          description = r.Name;
          type = "Stock Pull";
        }

        if (r.Type == "Misc Rectangular" && r.Name.StartsWith("BOXG10")) {
          description = r.Name;
          type = "Stock Pull";
        }

        if (r.Type == "Misc Round Oval" && r.Name.StartsWith("Metal Cs")) {
          description = "SLEEVE - TUBE";
          type = "Stock Pull";
        }

        if (r.Type == "Stock Pull" && r.Name.StartsWith("Bel")) {
          description = r.Name;
        }

        if (type == "Purchased") {
          string temp;
          if (purchasesMap.TryGetValue(r.Description, out temp)) {
            description = temp;
          } else {
            description = r.Description;
          }
          // type = "Duct Accessory";
        }

        if (type == "Assembly") {
          description = r.Name;
        }

        string po = r.PO == null || r.PO == "" ?
          r.JobName : r.PO;

        if (masterPO == null && po.Length > 25) {
          bool isTooLong = true;
          while (isTooLong) {
            masterPO = UIHelper.UserInputLoop($"Given PO number of: {po} is too long. Please enter a shortened version (under 25 characters).");
            if (masterPO.Length <= 25) {
              isTooLong = false;
            }
          }
        }

        // only for straight duct/pipe non-stock e.g. D/DS/SP/OSP
        double pieceLength = 0.00;
        if (r.Name == "D" || r.Description == "D" || r.Name == "DS" || r.Description == "DS" || r.Name == "DSCC" || r.Description == "DSCC") {
          pieceLength = Convert.ToDouble(r.Dims3);
        }
        if (r.Name == "SP" || r.Description == "SP") {
          pieceLength = Convert.ToDouble(r.Dims2);
        }
        if (r.Name == "OSP" || r.Description == "OSP") {
          pieceLength = Convert.ToDouble(r.Dims3);
        }
        if (r.Type == "Stock Pull" && description.StartsWith("STOCK SPIRAL:SP")) {
          pieceLength = 120.00;
        }
        if (r.Type == "Warm Air Stock" && description.StartsWith("SNAPLOCK:SLP")) {
          pieceLength = 60.00;
        }

        double skinWeight = r.SkinWeight == "" ? 0.00 : Convert.ToDouble(r.SkinWeight);
        double accessoryCost = r.VaneMatCost + r.ConnectorMatCost + r.StiffenerMatCost;

        cls.Add(new LineItem(customer,
                            r.SalesOrder,
                            masterPO == null ? po : masterPO,
                            type,
                            r.Quantity,
                            description,
                            r.Gauge,
                            r.Material,
                            r.MaterialWeight,
                            r.Weight,
                            r.Material2, // liner mat
                            r.LinerArea, // sq footage of liner
                            r.SkinGauge,
                            r.SkinMaterial,
                            skinWeight,
                            accessoryCost,
                            r.Rate,
                            false,
                            r.PriceEach,
                            pieceLength,
                            false, // is catalogue
                            0.00));
      }
      return cls;
    }

    private static string LookupFittingType(List<Fitting> fittings, string f) {
      foreach (var fcheck in fittings) {
        if (fcheck.Name == f) {
          return fcheck.ProductGroup;
        }
      }
      // default is original item name
      return f;
    }

    private static string SanitizeDim(string d) {
      return (d == "0" ? "" : d);
    }

    private static List<T> ReadCsv<T>(string file, bool hasHeader=false) {
      using (var reader = File.OpenText(file))
      using (var csvReader = new CsvReader(reader, CultureInfo.CurrentCulture))
      {
        csvReader.Configuration.HasHeaderRecord = hasHeader;
        csvReader.Configuration.IgnoreBlankLines = true;
        csvReader.Configuration.Delimiter = ",";
        csvReader.Configuration.PrepareHeaderForMatch = (header, index) => header
          .Replace(" ", "")
          .Replace("#", "");
        var lines = csvReader.GetRecords<T>();
        return lines.ToList();
      }
    }

    private static List<RawCamDuctCSVLine> ReadRawCamDuctCSV(string file) {
      return ReadCsv<RawCamDuctCSVLine>(file, false);
    }

    private static List<DimOptionLine> ReadDimsCSV() {
      return ReadCsv<DimOptionLine>("helper_dimension_options.csv", true);
    }

    private static List<ConnRename> ReadConnRenameCSV() {
      return ReadCsv<ConnRename>("helper_connector_rename.csv", true);
    }

    private static List<string> SanitizeConnectors(RawCamDuctCSVLine r) {
      List<string> conns = new List<string>{r.Connector1,
                                            r.Connector2,
                                            r.Connector3,
                                            r.Connector4};
      var connRenames = ReadConnRenameCSV();
      foreach (var cr in connRenames) {
        for (int i = 0; i < conns.Count; i++) {
          if (conns[i] == cr.FullName) {
            conns[i] = cr.AbbvName;
          }
        }
      }
      return conns;
    }

    public static void DebugPrintCleanCSV(List<LineItem> cls, string fname) {
      using (var w = new StreamWriter("./" + fname))
      using (var csvWriter = new CsvWriter(w, CultureInfo.InvariantCulture))
      {
        csvWriter.WriteRecords(cls);
      }
      return;
    }
  }
}
