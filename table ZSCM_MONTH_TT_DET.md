# Table Definition: ZSCM_MONTH_TT_DET

## Overview
Child/Detail table to store individual shipment details used for calculating monthly cumulative distance per Transport Truck/Vehicle. This table provides an audit trail and reference for the aggregated data in parent table ZSCM_MONTH_TT_DIST.

## Table Relationship

```
ZSCM_MONTH_TT_DIST (Parent - Aggregated Monthly Distance)
    │
    └── ZSCM_MONTH_TT_DET (Child - Shipment Details)
        Key: VEHL_NO + GJAHR + MONAT + TKNUM
```

## Table Properties

| Property | Value |
|----------|-------|
| Table Name | ZSCM_MONTH_TT_DET |
| Short Description | Monthly TT Distance - Shipment Details |
| Delivery Class | A (Application Table) |
| Data Class | APPL1 |
| Size Category | 3 |
| Buffering | Not buffered |

## Field Definition

| Field | Data Element | Type | Length | Dec | Key | Description |
|-------|--------------|------|--------|-----|-----|-------------|
| MANDT | MANDT | CLNT | 3 | 0 | X | Client |
| VEHL_NO | YTRUCK_NO | CHAR | 15 | 0 | X | Vehicle/Truck Number |
| GJAHR | GJAHR | NUMC | 4 | 0 | X | Fiscal Year |
| MONAT | MONAT | NUMC | 2 | 0 | X | Month |
| TKNUM | TKNUM | CHAR | 10 | 0 | X | Shipment Number |
| MFRGR | MFRGR | CHAR | 8 | 0 | | Material Freight Group |
| LIFNR | LIFNR | CHAR | 10 | 0 | | Transporter Vendor |
| RO_OUT_DATE | DATS | DATS | 8 | 0 | | RO Out Date |
| ROUTE | ROUTE | CHAR | 6 | 0 | | Route Code (legacy, not used in v2.0) |
| DISTANCE | ZDIST_SHIP | DEC | 15 | 3 | | Shipment Distance (from OIGSS) |
| DISTANCE_UNIT | MEINS | UNIT | 3 | 0 | | Distance Unit |
| CREATED_BY | ERNAM | CHAR | 12 | 0 | | Created By |
| CREATED_ON | ERDAT | DATS | 8 | 0 | | Created Date |
| CREATED_TM | ERZET | TIMS | 6 | 0 | | Created Time |

## Technical Key
Primary Key: MANDT + VEHL_NO + GJAHR + MONAT + TKNUM

## Foreign Key Relationship

| Check Table | Check Fields | Foreign Key Fields |
|-------------|--------------|-------------------|
| ZSCM_MONTH_TT_DIST | MANDT, VEHL_NO, GJAHR, MONAT | MANDT, VEHL_NO, GJAHR, MONAT |

## Custom Data Elements (to be created if not existing)

### ZDIST_SHIP
| Property | Value |
|----------|-------|
| Short Description | Shipment Distance |
| Domain | ZDIST_SHIP or use existing decimal domain |
| Data Type | DEC |
| Length | 15 |
| Decimals | 3 |

## Technical Settings
- Delivery Class: A
- Data Class: APPL1
- Size Category: 3 (larger than parent due to more records)
- Buffering: Not buffered

## SE11 Creation Steps

1. Go to transaction SE11
2. Enter table name: ZSCM_MONTH_TT_DET
3. Click Create
4. Enter Short Description: "Monthly TT Distance - Shipment Details"
5. Set Delivery Class: A
6. Set Data Browser/Table View Maint.: Display/Maintenance Allowed
7. Add fields as per the field definition above
8. Create Foreign Key:
   - Position on VEHL_NO field
   - Click Foreign Key button
   - Check Table: ZSCM_MONTH_TT_DIST
   - Map fields: MANDT→MANDT, VEHL_NO→VEHL_NO, GJAHR→GJAHR, MONAT→MONAT
9. Go to Technical Settings tab:
   - Data Class: APPL1
   - Size Category: 3
   - Buffering: Not buffered
10. Activate the table

## Index Recommendations

### Secondary Index: ZSCM_MONTH_TT_DET~01
| Field | Description |
|-------|-------------|
| TKNUM | For lookup by shipment number |

### Secondary Index: ZSCM_MONTH_TT_DET~02
| Field | Description |
|-------|-------------|
| RO_OUT_DATE | For date-based queries |

## Sample Data

| VEHL_NO | GJAHR | MONAT | TKNUM | MFRGR | LIFNR | RO_OUT_DATE | ROUTE | DISTANCE |
|---------|-------|-------|-------|-------|-------|-------------|-------|----------|
| MH12AB1234 | 2025 | 12 | 0000012345 | FG001 | 0000001001 | 20251205 | RT0001 | 450.500 |
| MH12AB1234 | 2025 | 12 | 0000012346 | FG001 | 0000001001 | 20251210 | RT0002 | 320.000 |
| MH12AB1234 | 2025 | 12 | 0000012347 | FG002 | 0000001002 | 20251215 | RT0001 | 450.500 |

