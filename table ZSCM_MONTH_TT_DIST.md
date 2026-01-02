# Table Definition: ZSCM_MONTH_TT_DIST

## Overview
Custom table to store monthly cumulative distance travelled by each Transport Truck/Vehicle.

## Table Properties
| Property | Value |
|----------|-------|
| Table Name | ZSCM_MONTH_TT_DIST |
| Short Description | Monthly Transport Truck Distance |
| Delivery Class | A (Application Table) |
| Data Class | APPL1 |
| Size Category | 2 |
| Buffering | Not buffered |

## Field Definition

| Field | Data Element | Type | Length | Dec | Key | Description |
|-------|--------------|------|--------|-----|-----|-------------|
| MANDT | MANDT | CLNT | 3 | 0 | X | Client |
| VEHL_NO | YTRUCK_NO | CHAR | 15 | 0 | X | Vehicle/Truck Number |
| GJAHR | GJAHR | NUMC | 4 | 0 | X | Fiscal Year |
| MONAT | MONAT | NUMC | 2 | 0 | X | Month |
| TOTAL_DISTANCE | ZDIST_TOT | DEC | 15 | 3 | | Total Distance Travelled |
| DISTANCE_UNIT | MEINS | UNIT | 3 | 0 | | Distance Unit (KM) |
| TRIP_COUNT | ZTRIP_CNT | INT4 | 10 | 0 | | Number of Trips |
| LIFNR | LIFNR | CHAR | 10 | 0 | | Primary Transporter Vendor |
| CREATED_BY | ERNAM | CHAR | 12 | 0 | | Created By |
| CREATED_ON | ERDAT | DATS | 8 | 0 | | Created Date |
| CREATED_TM | ERZET | TIMS | 6 | 0 | | Created Time |
| CHANGED_BY | AENAM | CHAR | 12 | 0 | | Changed By |
| CHANGED_ON | AEDAT | DATS | 8 | 0 | | Changed Date |
| CHANGED_TM | AEZET | TIMS | 6 | 0 | | Changed Time |

## Technical Key
Primary Key: MANDT + VEHL_NO + GJAHR + MONAT

## Custom Data Elements (to be created if not existing)

### ZDIST_TOT
| Property | Value |
|----------|-------|
| Short Description | Total Distance Travelled |
| Domain | Create new domain ZDIST_TOT |
| Data Type | DEC |
| Length | 15 |
| Decimals | 3 |

### ZTRIP_CNT
| Property | Value |
|----------|-------|
| Short Description | Number of Trips |
| Domain | INT4 |
| Data Type | INT4 |
| Length | 10 |

## SE11 Creation Steps

1. Go to transaction SE11
2. Enter table name: ZSCM_MONTH_TT_DIST
3. Click Create
4. Enter Short Description: "Monthly Transport Truck Distance"
5. Set Delivery Class: A
6. Set Data Browser/Table View Maint.: Display/Maintenance Allowed
7. Add fields as per the field definition above
8. Go to Technical Settings tab:
   - Data Class: APPL1
   - Size Category: 2
   - Buffering: Not buffered
9. Activate the table

## Index Recommendations
No additional secondary indexes required. Primary key provides sufficient access path.

