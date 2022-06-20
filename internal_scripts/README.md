# Pricing Master Script

NEEDS UPDATE

The following provides both user directions and some small developer notes regarding this script.
The script provides the primary functions related to quoting, billing, project billing, stock order
(inputs), BD order writing and billing, and purchase orders. The majority of the options
prompted for at the start of the script involve and require both QB and CAMDuct. **QB requires the SDK
installed locally.**

This document also has basic instructions on "proper" billing procedure (needs update).

## General Notes

Perhaps the most important things to keep in mind are the following:

1. Read the script carefully - updates occur!
2. Sanity check the results - edge cases exist!

## Option (0) - Billing

- QB required

This option is used for all FS and FL orders. It takes the CAM information, prices and writes the data into
the ERP, and then writes the sales order with the updated pricing into QB for later invoicing. There are a few
additional prompts of which to be aware:

- pricing catalog -> whether or not to use a fixed pricing list for matching items on this order
- `simple` or `line item` -> lot (grouped) or line item billing
- `FL` or `FS` order type -> `FS` will be a **non-quoted** order, both simple and line item
  - `FS` listens for either a `simple` or `line item` output from CAM
  - `FL` then prompts for the quoted order number, which is in place from following the quote steps

The `FL` option does NOT require re-export from CAM. It pulls the previously written data from the ERP.

- the script "hears" a new CSV file written from CAM
- a prompt for `LDS` will show up for each new file
  - this prompts the ERP to add an additional line item (`Misc Charge`) with the amount for LDS (currently X%)

### Billing Specifics

**Much of this information also pertains to quotes.**

1. J Supply (and distributors)

Some customers have fixed pricing lists that are not fixed items, but rather item dimensional categories. As such,
no option (yet) exists to properly support automating this price lookup. Manual intervention is required to ensure prices match.
The `Distributor Pricing List` should be available for accurate quoting and billing. It is strongly recommended to default to line
item billing, as this makes matching item prices easy rather than a very intensive task.

2. Roof Curbs, Chase Tops, Etc.

These specific items have "baked-in" margin, so the ERP tends to price these lower than we want to sell them at. Always verify/match
prices where needed.

3. T, Custom Assemblies, Etc.

There are a few customers that have specific methods that do not currently involve the ERP, such as DB. Work would need to
be done to provide support down the line.

4. Fixed Pricing Lists

At the moment, the following are customers that use fixed pricing lists:

- J and distributors (see above)
- J Manufacturing (for J pipe)
- BD
- S

The current lists and supported customers can be found via the ERP -> Pricing Lists.

## Option (1) - Quote

- QB required

This option is used for both simple and line item quotes. It has similar prompts to billing, just with fewer options.

Take special care to check for proper pricing behavior in QB, as quotes have strange "quirks" compared to sales orders,
especially in regard to stock sprial items and purchase items (i.e. non-lot items).

Below is a summary of the current quoting process:

### Quote Process (QL -> FL)

1 November 2021, hoddr

First and foremost always read carefully and check for significant price discrepancies between
CAM and the ERP! Items where this is the case must be brought to admin attention to permit
adjusting prices on certain items or in general to maintain proper margins!

1. Enter quoted list into CAM with ALL items (including flat stock, angles, etc as misc charges)
2. Start billing script AND QB.
3. Choose quote, then simple or line item per needs.
4. When waiting for file change pops up, export the correct quote type from CAM.
5. Follow the prompts for the process.
6. Verify the quote was written correctly to the ERP; check the line items for accuracy.
7. Verify the quote was written to QB correct; check the line items for accuracy and pricing.
8. At the moment, stock spiral is still a lookup item within QB, so those prices will vary AND
the pricing rules (discounts) are still applied.
9. Send quote as normal to customer.
10. Upon receipt of PO, create an order in the orders app as normal, noting order number and PO.
11. Open the quote in the ERP and choose "Create order (FL)". Enter the correct order number and PO
from the previous step.
12. Write both the quote number and the order number on the billing copies, noting that the order
is an FL.
13. Hand to scheduler.
14. Place in normal billing pile now!
15. Billed via script (billing option -> simple/line item -> FL -> enter order number -> retrieves data
from ERP rather than CAM). No need to re-export.

## Option (2) - Stock Order Input

- QB required

This option is used to `add` stock spiral to on-floor inventory. It requires a CAM job with the correct
number and types of stock spiral items written. It will then prompt for a line item billing export from CAM.
This script option will write the order as a specific order type, having the ERP remove the required material
to "construct" the stock spiral items indicated.

## Option (3) - Project

This option is only for project management. It prompts for a billing line item export from CAM.
A key note to remember is: if the project PO includes (i.e. covers) LDS, do NOT say "yes" to the LDS prompt.
This would double-bill the customer! You will also be prompted to indicate the project by number. These orders
will be written as non-complete and non-billed, requiring further work within the ERP.

## Option (4) - Project - Bill

- QB required

This option is only for project management. It prompts for the project number. If there are any extras
present, the script will prompt for a sales order number for which to write any found extras.
It will output a total amount to write for progress billing as well as the total weight.

## Option (5) - BD - Write

This option is used for writing a BD order into the ERP for "progressive" billing. This permits
tracking the fabrication of each piece (i.e. building assemblies like in QB) for "live" accounting
purposes. This also prompts to export a line item billing CSV.

## Option (6) - BD - Bill

This option is use for taking a complete (in ERP) BD order and writing the data to a sales order/invoice in QB
for invoicing. Note that each line item in the order must have been fabricated in full in the ERP.

## Option (7) - Purchase Orders

This option is for taking purchase orders from the ERP into QB. While other options are supported, only steel
is being used at this time. Note that the subsequent PO number (driven by QB) should be set correctly in the ERP
prior to writing.
