/*
 * Holds helper functions for any UI feedback/reading.
 *
 * hoddr, Feb 2021
 */

using System;

namespace master {
  public class UIHelper {
    public static void SectionDelimiter() {
      Console.WriteLine("\n----------------------------------------\n");
    }

    public static string UserInputLoop(string printMsg, bool runCheck=true) {
      bool IsCorrect() {
        string t = Console.ReadLine();
        return !(t == "n" || t == "N");
      }

      bool isDone = false;
      string s = null;
      while (!isDone) {
        Console.WriteLine($"{printMsg}\n");
        s = Console.ReadLine();
        if (runCheck && s != "") {
          Console.WriteLine($"You entered:\n\n{s}\nIf this is NOT correct, enter (N/n) to retry. Otherwise, hit any button to continue.");
          bool isCorrect = IsCorrect();
          if (isCorrect) {
            isDone = true;
          }
        } else {
          if (s != "") {
            Console.WriteLine($"You entered:\n\n{s}\n");
          }
          isDone = true;
        }
      }
      return s;
    }
  }
}
