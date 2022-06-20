using System;
using System.Collections.Generic;
using System.Linq;
using System.Net.Http;
using System.Net.Http.Json;
using System.Net.Http.Headers;
using System.Text;
using System.Text.Json;
using System.Threading.Tasks;

namespace master {
  public class Client {
    static string tokenuuid = "fcd314b1-4f80-41c3-bef0-fefa28d9bb9f";
    static JsonSerializerOptions opts = new() { PropertyNameCaseInsensitive = true };
    static JsonSerializerOptions optsWrite = new() { PropertyNamingPolicy = JsonNamingPolicy.CamelCase };

    private static HttpClient GenClient() {
      // this handler for local only!
      // var handler = new HttpClientHandler() {
      //   ServerCertificateCustomValidationCallback = HttpClientHandler.DangerousAcceptAnyServerCertificateValidator
      // };
      // var client = new HttpClient(handler);
      var client = new HttpClient();
      client.DefaultRequestHeaders.Authorization = new AuthenticationHeaderValue("Bearer", tokenuuid);
      return client;
    }

    private static async Task<HttpResponseMessage> GetRequest(HttpClient client, string url) {
      try {
        HttpResponseMessage resp = await client.GetAsync(url);
        Console.WriteLine($"{(resp.IsSuccessStatusCode ? "Success" : "Error")} - {resp.StatusCode}");
        resp.EnsureSuccessStatusCode();
        return resp;
      } catch (Exception e) {
        Console.WriteLine($"Get exception: {e}.");
        return null;
      }
    }

    private static async Task<HttpResponseMessage> PutRequest(HttpClient client, string url, StringContent httpContent) {
      try {
        HttpResponseMessage resp = await client.PutAsync(url, httpContent);
        Console.WriteLine($"{(resp.IsSuccessStatusCode ? "Success" : "Error")} - {resp.StatusCode}");
        resp.EnsureSuccessStatusCode();
        return resp;
      } catch (Exception e) {
        Console.WriteLine($"Put exception: {e}.");
        return null;
      }
    }

    private static async Task<HttpResponseMessage> PostRequest(HttpClient client, string url, StringContent httpContent) {
      try {
        HttpResponseMessage resp = await client.PostAsync(url, httpContent);
        Console.WriteLine($"{(resp.IsSuccessStatusCode ? "Success" : "Error")} - {resp.StatusCode}");
        if (!resp.IsSuccessStatusCode) {
          try {
            var resContent = await resp.Content.ReadAsStringAsync();
            Console.WriteLine($"API Error: {resContent}");
          } catch (Exception ex) {
            Console.WriteLine($"API Error parse exception: {ex}");
          }
          return null;
        }
        return resp;
      } catch (Exception e) {
        Console.WriteLine($"Post exception: {e}.");
        return null;
      }
    }

    public static async Task<Project> RetrieveProjectData(string appId) {
      var client = GenClient();
      string url = $"{Types.baseURI}/projects/appId/{appId}";
      var res = await GetRequest(client, url);
      if (res != null) {
        var resContent = await res.Content.ReadAsStringAsync();
        var prj = JsonSerializer.Deserialize<Project>(resContent, opts);
        return prj;
      } else {
        Console.WriteLine("Error retrieving project sections. Check for error messages or retry with correct project app id.");
        return null;
      }
    }

    public static async Task<List<LineItem>> RunPricing(string url, List<LineItem> clis) {
      var stringPayload = JsonSerializer.Serialize<List<LineItem>>(clis, optsWrite);
      var httpContent = new StringContent(stringPayload, Encoding.UTF8, "application/json");
      var client = GenClient();
      var res = await PostRequest(client, url, httpContent);
      if (res != null) {
        var resContent = await res.Content.ReadAsStringAsync();
        var pricedLis = JsonSerializer.Deserialize<List<LineItem>>(resContent, opts);
        return pricedLis;
      } else {
        Console.WriteLine($"Failed to post order.");
        return null;
      }
    }

    public static async Task<List<PricingList>> GetPricingLists() {
      string url = $"{Types.baseURI}/pricing-lists";
      var client = GenClient();
      var res = await GetRequest(client, url);
      if (res != null) {
        var resContent = await res.Content.ReadAsStringAsync();
        var lists = JsonSerializer.Deserialize<List<PricingList>>(resContent, opts);
        return lists;
      } else {
        Console.WriteLine("Failed to retrieve pricing lists.");
        return null;
      }
    }

    public static async Task<Filtered> DoProjectBilling(string url) {
      var client = GenClient();
      var httpContent = new StringContent("{}", Encoding.UTF8, "application/json");
      var res = await PostRequest(client, url, httpContent);
      if (res != null) {
        var resContent = await res.Content.ReadAsStringAsync();
        var f = JsonSerializer.Deserialize<Filtered>(resContent, opts);
        return f;
      } else {
        Console.WriteLine("Failed to complete and return project billing results.");
        return null;
      }
    }

    public static async Task<int> MarkInvoiceNumberODL(string invNumber, string orderId) {
      string url = $"https://app.com/api/job/{orderId}/invoice-number?invoiceNumber={invNumber}&key=sdfkjweoijfskdf-qlkjdflsdkj123-2321234";
      var emptyHttpContent = new StringContent("{}", Encoding.UTF8, "application/json");
      var client = GenClient();
      client.DefaultRequestHeaders.Accept.Add(new MediaTypeWithQualityHeaderValue("application/json"));
      var res = await PutRequest(client, url, emptyHttpContent);
      if (res != null) {
        return 0; // simple success/error code
      }
      return 1; // simple success/error code
    }

    public static async Task<int> MarkInvoiceNumber(string invNumber, string orderNumber) {
      string url = $"{Types.baseURI}/orders/number/{orderNumber}/bill/{invNumber}";
      var emptyHttpContent = new StringContent("{}", Encoding.UTF8, "application/json");
      var client = GenClient();
      client.DefaultRequestHeaders.Accept.Add(new MediaTypeWithQualityHeaderValue("application/json"));
      var res = await PutRequest(client, url, emptyHttpContent);
      if (res != null) {
        return 0; // simple success code
      }
      return 1;
    }

    public static async Task<List<LineItem>> GetFabricatedListItems(string orderNumber) {
      string url = $"{Types.baseURI}/orders/number/{orderNumber}/FL";
      var client = GenClient();
      client.DefaultRequestHeaders.Accept.Add(new MediaTypeWithQualityHeaderValue("application/json"));
      var res = await GetRequest(client, url);
      if (res != null) {
        var resContent = await res.Content.ReadAsStringAsync();
        var lis = JsonSerializer.Deserialize<List<LineItem>>(resContent, opts);
        return lis;
      } else {
        Console.WriteLine("Failed to retrieve fabricated list items.");
        return null;
      }
    }

    public static async Task<int> PostBDOrder(List<LineItem> lis) {
      string url = $"{Types.baseURI}/progressive/orders";
      var stringPayload = JsonSerializer.Serialize<List<LineItem>>(lis, optsWrite);
      var httpContent = new StringContent(stringPayload, Encoding.UTF8, "application/json");
      var client = GenClient();
      var res = await PostRequest(client, url, httpContent);
      if (res != null) {
        return 1;
      }
      return 0;
    }

    public static async Task<PurchaseOrder> GetPurchaseOrder(string url) {
      var client = GenClient();
      client.DefaultRequestHeaders.Accept.Add(new MediaTypeWithQualityHeaderValue("application/json"));
      var res = await GetRequest(client, url);
      if (res != null) {
        var resContent = await res.Content.ReadAsStringAsync();
        var po = JsonSerializer.Deserialize<PurchaseOrder>(resContent, opts);
        return po;
      } else {
        Console.WriteLine("Failed to retrieve purchase order.");
        return null;
      }
    }
  }
}
