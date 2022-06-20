/*
 * Contains logic for aggregating simple billing groups and record(s).
 *
 * hoddr, Feb 2021
 */

using System;
using System.Collections.Generic;
using System.Linq;

namespace master {
  public class Simple {
    public static List<SimpleGroup> Aggregate(List<LineItem> lis) {
      List<SimpleGroup> groupings = new List<SimpleGroup>();
      string orderNumber = lis[0].OrderNumber;
      string po = lis[0].PO;
      string customer = lis[0].Customer;
      string[] types = {"Rectangular Duct", "Round Duct", "Oval Duct", "Duct Accessory", "Misc Rectangular", "Misc Round Oval"};
      foreach (string type in types) {
        double runningPrice = 0.0;
        int runningWeight = 0;
        foreach (var li in lis) {
          if (li.Category == type && !li.Name.StartsWith("J PIPE")) {
            runningPrice += li.PriceEach * li.Quantity;
            runningWeight += Convert.ToInt32(li.Weight);
          }
        }
        groupings.Add(new SimpleGroup(orderNumber,
                                      customer,
                                      po,
                                      type,
                                      type,
                                      runningWeight,
                                      Math.Round(runningPrice, 2),
                                      1));
      }

      // gather up j pipe
      var filteredJPipe = lis.Where(li => li.Category == "Stock Pull" && li.Name.StartsWith("J PIPE")).ToList();
      foreach (var li in filteredJPipe) {
        groupings.Add(new SimpleGroup(orderNumber,
                                      customer,
                                      po,
                                      "Round Duct",
                                      li.Name,
                                      Convert.ToInt32(li.Weight * li.Quantity),
                                      Math.Round(li.PriceEach * li.Quantity, 2),
                                      1));
      }

      // add in stock items
      var filteredStockItems = lis.Where(li => li.Category == "Stock Pull" && !(li.Name.StartsWith("J PIPE") || li.Name.StartsWith("SLD1") || li.Name.StartsWith("BOXG10") || li.Name.StartsWith("SLEEVE - TUBE"))).ToList();
      foreach (var li in filteredStockItems) {
        groupings.Add(new SimpleGroup(orderNumber,
                                      customer,
                                      po,
                                      "Manufactured Stock",
                                      li.Name,
                                      Convert.ToInt32(li.Weight * li.Quantity),
                                      Math.Round(li.PriceEach, 2),
                                      li.Quantity));
      }

      // add in sprial duct liner
      var filteredSDLItems = lis.Where(li => li.Category == "Stock Pull" && li.Name.StartsWith("SLD1")).ToList();
      foreach (var li in filteredSDLItems) {
        groupings.Add(new SimpleGroup(orderNumber,
                                      customer,
                                      po,
                                      "Manufactured Stock",
                                      li.Name,
                                      Convert.ToInt32(li.Weight * li.Quantity),
                                      Math.Round(li.PriceEach, 2),
                                      li.Quantity));
      }

      // add in spot boxes
      var filteredSpotBoxItems = lis.Where(li => li.Category == "Stock Pull" && li.Name.StartsWith("BOXG10")).ToList();
      foreach (var li in filteredSpotBoxItems) {
        groupings.Add(new SimpleGroup(orderNumber,
                                      customer,
                                      po,
                                      "Misc Rectangular",
                                      li.Name,
                                      Convert.ToInt32(li.Weight * li.Quantity),
                                      Math.Round(li.PriceEach, 2),
                                      li.Quantity));
      }

      // add in metal sleeve tubes
      var filteredMetItems = lis.Where(li => li.Category == "Stock Pull" && li.Name.StartsWith("SLEEVE - TUBE")).ToList();
      foreach (var li in filteredMetItems) {
        groupings.Add(new SimpleGroup(orderNumber,
                                      customer,
                                      po,
                                      "Misc Round Oval",
                                      li.Name,
                                      Convert.ToInt32(li.Weight * li.Quantity),
                                      Math.Round(li.PriceEach, 2),
                                      li.Quantity));
      }

      // add in misc charges
      var filteredChargeItems = lis.Where(li => li.Category == "Misc Charge").ToList();
      foreach (var li in filteredChargeItems) {
        groupings.Add(new SimpleGroup(orderNumber,
                                      customer,
                                      po,
                                      "Misc Charge",
                                      li.Name,
                                      Convert.ToInt32(li.Weight * li.Quantity),
                                      Math.Round(li.PriceEach, 2),
                                      li.Quantity));
      }

      // add in freight
      var filteredFreightItems = lis.Where(li => li.Category == "Freight").ToList();
      foreach (var li in filteredFreightItems) {
        groupings.Add(new SimpleGroup(orderNumber,
                                      customer,
                                      po,
                                      "FREIGHT",
                                      li.Name,
                                      Convert.ToInt32(0.0), // weight
                                      Math.Round(li.PriceEach, 2),
                                      li.Quantity));
      }

      // add in pure labor charges
      var filteredLaborItems = lis.Where(li => li.Category == "Labor").ToList();
      foreach (var li in filteredLaborItems) {
        groupings.Add(new SimpleGroup(orderNumber,
                                      customer,
                                      po,
                                      "Labor",
                                      li.Name,
                                      Convert.ToInt32(0.0), // weight
                                      Math.Round(li.PriceEach, 2),
                                      li.Quantity));
      }


      // add in snaplock/warm air pipe
      var filteredSnaplockItems = lis.Where(li => li.Category == "Warm Air Stock").ToList();
      foreach (var li in filteredSnaplockItems) {
        groupings.Add(new SimpleGroup(orderNumber,
                                      customer,
                                      po,
                                      "Warehouse Item",
                                      li.Name,
                                      Convert.ToInt32(li.Weight * li.Quantity),
                                      Math.Round(li.PriceEach, 2),
                                      li.Quantity));
      }

      // add in purchased flanges
      var filteredFlangeItems = lis.Where(li => li.Category == "Purchased" && li.Name.StartsWith("FLANGES")).ToList();
      foreach (var li in filteredFlangeItems) {
        groupings.Add(new SimpleGroup(orderNumber,
                                      customer,
                                      po,
                                      "Warehouse Item",
                                      li.Name,
                                      Convert.ToInt32(li.Weight * li.Quantity),
                                      Math.Round(li.PriceEach, 2),
                                      li.Quantity));
      }

      // add in purchased hetos
      var filteredHETOItems = lis.Where(li => li.Category == "Purchased" && li.Name.StartsWith("HETO")).ToList();
      foreach (var li in filteredHETOItems) {
        groupings.Add(new SimpleGroup(orderNumber,
                                      customer,
                                      po,
                                      "Warehouse Item",
                                      li.Name,
                                      Convert.ToInt32(li.Weight * li.Quantity),
                                      Math.Round(li.PriceEach, 2),
                                      li.Quantity));
      }

      // add in assemblies (Big Dutchman)
      var filteredAssemblies = lis.Where(li => li.Category == "Assembly").ToList();
      foreach (var li in filteredAssemblies) {
        groupings.Add(new SimpleGroup(orderNumber,
                                      customer,
                                      po,
                                      "Manufactured Stock",
                                      li.Name,
                                      Convert.ToInt32(li.Weight * li.Quantity),
                                      Math.Round(li.PriceEach, 2),
                                      li.Quantity));
      }

      var filteredPurchaseItems = lis.Where(li => li.Category == "Purchased" && !(li.Name.StartsWith("FLANGE") || li.Name.StartsWith("HETO"))).ToList();
      foreach (var li in filteredPurchaseItems) {
        groupings.Add(new SimpleGroup(orderNumber,
                                      customer,
                                      po,
                                      "Warehouse Item",
                                      li.Name,
                                      Convert.ToInt32(li.Weight * li.Quantity),
                                      Math.Round(li.PriceEach, 2),
                                      li.Quantity));
      }

      var filteredCurbs = lis.Where(li => li.Category == "Curb").ToList();
      foreach (var li in filteredCurbs) {
        groupings.Add(new SimpleGroup(orderNumber,
                                      customer,
                                      po,
                                      "Curb Adapter",
                                      li.Name,
                                      Convert.ToInt32(li.Weight * li.Quantity),
                                      Math.Round(li.PriceEach, 2),
                                      li.Quantity));
      }
      var filteredFastPass = lis.Where(li => li.Category == "Fast Pass").ToList();
      foreach (var li in filteredFastPass) {
        groupings.Add(new SimpleGroup(orderNumber,
                                      customer,
                                      po,
                                      "Fast Pass",
                                      li.Name,
                                      Convert.ToInt32(li.Weight * li.Quantity),
                                      Math.Round(li.PriceEach, 2),
                                      li.Quantity));
      }
      return groupings;
    }
  }
}
