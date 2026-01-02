# Implementation Guide: ZSCM_MONTHLY_TT_DISTANCE

## Overview
This guide provides step-by-step instructions to implement the Monthly Transport Truck Distance Calculation solution in SAP ECC 6.0.

## Version 2.0 Changes

**Program Version 2.0** includes the following major changes:

1. **Object-Oriented Programming**: Refactored from procedural (FORMs) to OOP using local class `lcl_report`
2. **New Shipment Selection Logic**:
   - Source: YTTSPOD → VBRP → LIPS → OIGSI (instead of ZTRSTLMNT)
   - Filter by RO_OUT_DATE within selected month and STATUS = 'C'
   - Get MFRGR from **LIPS** (Delivery Items) - not from VBRP
   - Validate Material Freight Group against ZLOG_EXEC_VAR configuration
   - Verify all deliveries of each shipment meet the conditions
3. **New Distance Source**: OIGSS table (instead of TVRO Route Master)
4. **NetWeaver 7.31 Compatibility**: No inline declarations, string templates, or other 7.40+ syntax
5. **Selection Screen**: Removed vendor filter (S_LIFNR), vehicle filter uses YTTSPOD-TRUCK_NO

### Data Flow (v2.0)

```
YTTSPOD (POD records)
  │ Filter: RO_OUT_DATE in month, STATUS = 'C'
  │ Get: BILLNO, TRUCK_NO, RO_OUT_DATE
  ▼
VBRP (Billing Items)
  │ Join: VBELN = YTTSPOD.BILLNO
  │ Get: VGBEL (Delivery Number)
  ▼
LIPS (Delivery Items)
  │ Join: VBELN = VBRP.VGBEL
  │ Filter: MFRGR IN ZLOG_EXEC_VAR config
  │ Get: MFRGR (Material Freight Group)
  ▼
OIGSI (Shipment-Delivery Assignment)
  │ Join: VBELN = VBRP.VGBEL
  │ Get: TKNUM (Shipment Number)
  ▼
Validate: All deliveries of each shipment
  │ Check: All deliveries have POD with STATUS = 'C'
  │        and RO_OUT_DATE in selected month
  ▼
OIGSS (Shipment Stage)
  │ Join: TKNUM = OIGSI.TKNUM
  │ Get: DISTZ (Distance), MEDST (Unit)
  │ Note: First record per shipment
  ▼
Aggregate by TRUCK_NO (Vehicle)
  │ Sum: Total Distance
  │ Count: Trip Count
  │ Determine: Primary Vendor
  ▼
ZSCM_MONTH_TT_DIST (Header Table)
ZSCM_MONTH_TT_DET (Detail Table)
  ▼
REST API → Fr8first Platform
```

### Key Tables Used

| Table | Description | Key Fields Used |
|-------|-------------|-----------------|
| YTTSPOD | Proof of Delivery | BILLNO, RO_OUT_DATE, STATUS, TRUCK_NO |
| VBRP | Billing Document Items | VBELN, VGBEL |
| LIPS | Delivery Items | VBELN, MFRGR |
| OIGSI | Shipment-Delivery Assignment | TKNUM, VBELN |
| OIGSS | Shipment Stage (Distance) | TKNUM, DISTZ, MEDST |
| ZLOG_EXEC_VAR | Configuration | NAME, MFRGR, ACTIVE, REMARKS |

---

## Implementation Checklist

- [ ] Step 1: Create Custom Data Elements (if needed)
- [ ] Step 2: Create Custom Table ZSCM_MONTH_TT_DIST (Header)
- [ ] Step 3: Create Custom Table ZSCM_MONTH_TT_DET (Detail/Child)
- [ ] Step 4: Create ABAP Program
- [ ] Step 5: Create Transaction Code (Optional)
- [ ] Step 6: Configure ZLOG_EXEC_VAR
- [ ] Step 7: Configure REST API Integration
- [ ] Step 8: Schedule Background Job (Optional)
- [ ] Step 9: Execute Test Cases
- [ ] Step 10: Transport to Quality/Production

---

## Step 1: Create Custom Data Elements (SE11)

### 1.1 Create Data Element: ZDIST_TOT

1. Go to transaction SE11
2. Enter Data Element: ZDIST_TOT
3. Click Create
4. Enter:
   - Short Description: Total Distance Travelled
   - Domain: Create new or use existing decimal domain
   - Data Type: DEC
   - Length: 15
   - Decimals: 3
5. Maintain Field Labels:
   - Short: Distance
   - Medium: Total Dist
   - Long: Total Distance Travelled
   - Heading: Total Distance
6. Activate

### 1.2 Create Data Element: ZTRIP_CNT

1. Go to transaction SE11
2. Enter Data Element: ZTRIP_CNT
3. Click Create
4. Enter:
   - Short Description: Number of Trips
   - Domain: INT4
   - Data Type: INT4
   - Length: 10
5. Maintain Field Labels:
   - Short: Trips
   - Medium: Trip Count
   - Long: Number of Trips
   - Heading: Trip Count
6. Activate

---

## Step 2: Create Custom Table ZSCM_MONTH_TT_DIST (SE11)

1. Go to transaction SE11
2. Enter Table: ZSCM_MONTH_TT_DIST
3. Click Create
4. Enter Short Description: "Monthly Transport Truck Distance"

### 2.1 Delivery and Maintenance Tab
- Delivery Class: A
- Data Browser/Table View Maint.: Display/Maintenance Allowed

### 2.2 Fields Tab

| Field | Key | Init | Data Element | Data Type | Length | Dec |
|-------|-----|------|--------------|-----------|--------|-----|
| MANDT | X | X | MANDT | CLNT | 3 | |
| VEHL_NO | X | | SIGNI | CHAR | 20 | |
| GJAHR | X | | GJAHR | NUMC | 4 | |
| MONAT | X | | MONAT | NUMC | 2 | |
| TOTAL_DISTANCE | | | ZDIST_TOT | DEC | 15 | 3 |
| DISTANCE_UNIT | | | MEINS | UNIT | 3 | |
| TRIP_COUNT | | | ZTRIP_CNT | INT4 | 10 | |
| LIFNR | | | LIFNR | CHAR | 10 | |
| CREATED_BY | | | ERNAM | CHAR | 12 | |
| CREATED_ON | | | ERDAT | DATS | 8 | |
| CREATED_TM | | | ERZET | TIMS | 6 | |
| CHANGED_BY | | | AENAM | CHAR | 12 | |
| CHANGED_ON | | | AEDAT | DATS | 8 | |
| CHANGED_TM | | | AEZET | TIMS | 6 | |

### 2.3 Currency/Quantity Fields Tab
- Reference for DISTANCE_UNIT: TOTAL_DISTANCE

### 2.4 Technical Settings
1. Click "Technical Settings" button
2. Enter:
   - Data Class: APPL1
   - Size Category: 2
   - Buffering: Not buffered
3. Save and return

### 2.5 Activate
- Click Activate (Ctrl+F3)

---

## Step 3: Create Child Table ZSCM_MONTH_TT_DET (SE11)

This child table stores shipment-level details for audit and reference.

1. Go to transaction SE11
2. Enter Table: ZSCM_MONTH_TT_DET
3. Click Create
4. Enter Short Description: "Monthly TT Distance - Shipment Details"

### 3.1 Delivery and Maintenance Tab
- Delivery Class: A
- Data Browser/Table View Maint.: Display/Maintenance Allowed

### 3.2 Fields Tab

| Field | Key | Init | Data Element | Data Type | Length | Dec |
|-------|-----|------|--------------|-----------|--------|-----|
| MANDT | X | X | MANDT | CLNT | 3 | |
| VEHL_NO | X | | SIGNI | CHAR | 20 | |
| GJAHR | X | | GJAHR | NUMC | 4 | |
| MONAT | X | | MONAT | NUMC | 2 | |
| TKNUM | X | | TKNUM | CHAR | 10 | |
| MFRGR | | | MFRGR | CHAR | 8 | |
| LIFNR | | | LIFNR | CHAR | 10 | |
| RO_OUT_DATE | | | DATS | DATS | 8 | |
| ROUTE | | | ROUTE | CHAR | 6 | |
| DISTANCE | | | ZDIST_SHIP | DEC | 15 | 3 |
| DISTANCE_UNIT | | | MEINS | UNIT | 3 | |
| CREATED_BY | | | ERNAM | CHAR | 12 | |
| CREATED_ON | | | ERDAT | DATS | 8 | |
| CREATED_TM | | | ERZET | TIMS | 6 | |

### 3.3 Foreign Key (Optional)
1. Position on VEHL_NO field
2. Click Foreign Key button
3. Check Table: ZSCM_MONTH_TT_DIST
4. Map: MANDT→MANDT, VEHL_NO→VEHL_NO, GJAHR→GJAHR, MONAT→MONAT

### 3.4 Technical Settings
- Data Class: APPL1
- Size Category: 3 (larger than header)
- Buffering: Not buffered

### 3.5 Activate
- Click Activate (Ctrl+F3)

---

## Step 4: Create ABAP Program (SE38)

1. Go to transaction SE38
2. Enter Program: ZSCM_MONTHLY_TT_DISTANCE
3. Click Create
4. Enter:
   - Title: Monthly Transport Truck Distance Calculation (TD Shipments)
   - Type: Executable program
   - Status: Test Program (change to Production later)
   - Application: V (Logistics)
5. Assign to a development package or $TMP
6. Copy the complete ABAP code from file: `ZSCM_MONTHLY_TT_DISTANCE.abap`
7. Activate

**Note:** 
- Route is fetched from OIGSV (TD Shipments) table and distance from TVRO (Route Master)
- The variant name for ZLOG_EXEC_VAR is defined as a constant: `GC_VARIANT_NAME = 'SCM_MONTH_TT_DIST'`
- Ensure ZLOG_EXEC_VAR has entries with NAME = 'SCM_MONTH_TT_DIST' and ACTIVE = 'X'

### 4.1 Maintain Text Elements

1. Go to: Goto → Text Elements → Selection Texts
2. Maintain:
   | Name | Text |
   |------|------|
   | P_MONAT | Month |
   | P_GJAHR | Year |
   | P_TEST | Test Mode |
   | S_VEHL | Vehicle Number (Truck) |

3. Go to: Goto → Text Elements → Text Symbols
4. Maintain:
   | ID | Text |
   |----|------|
   | 001 | Selection Parameters |
   | 002 | Optional Filters |
   | 003 | Processing Options |

5. Activate Text Elements

---

## Step 5: Create Transaction Code (SE93) - Optional

1. Go to transaction SE93
2. Enter Transaction: ZTTDIST
3. Click Create
4. Select: Program and selection screen (report transaction)
5. Enter:
   - Short Text: Monthly TT Distance Calculation
   - Program: ZSCM_MONTHLY_TT_DISTANCE
   - Selection Screen: 1000
6. Save

---

## Step 6: Configure ZLOG_EXEC_VAR

Ensure configuration entries exist for the applicable Material Freight Groups.

### 6.1 Check Existing Configuration

The program uses constant `GC_VARIANT_NAME = 'SCM_MONTH_TT_DIST'` to filter MFRGR entries.

```sql
SELECT * FROM zlog_exec_var
WHERE name = 'SCM_MONTH_TT_DIST'
  AND active = 'X'
```

### 6.2 Add Configuration Entries (SM30)

If entries don't exist, add them via SM30 or direct table maintenance:

1. Go to SM30
2. Enter Table: ZLOG_EXEC_VAR
3. Click Maintain
4. Add entries:

| NAME | NUMB | MFRGR | ACTIVE | REMARKS |
|------|------|-------|--------|---------|
| SCM_MONTH_TT_DIST | 0001 | MFRGR01 | X | Material Freight Group 1 |
| SCM_MONTH_TT_DIST | 0002 | MFRGR02 | X | Material Freight Group 2 |
| SCM_MONTH_TT_DIST | 0003 | MFRGR03 | X | Material Freight Group 3 |

Replace MFRGR01, MFRGR02, etc. with actual material freight group values.

**Important:** The NAME field must be exactly `SCM_MONTH_TT_DIST` as defined in the program constant.

---

## Step 7: Configure REST API Integration

The program sends calculated distance data to Fr8first platform via REST API after saving to database.

### 7.1 API URL Configuration in ZLOG_EXEC_VAR

The API Base URL is configured in `ZLOG_EXEC_VAR` table:

1. Go to SM30
2. Enter Table: ZLOG_EXEC_VAR
3. Click Maintain
4. Add entry:

| NAME | NUMB | ACTIVE | REMARKS |
|------|------|--------|---------|
| SCM_API_URL | 0001 | X | http://localhost:3000 |

**Important:**
- **NAME** field must be exactly `SCM_API_URL` (as defined in program constant `GC_API_URL_VAR`)
- **REMARKS** field stores the API Base URL (max 72 characters)
- For longer URLs, consider using ERRORMSG field (73 characters) or extend the table
- **ACTIVE** must be 'X' for the configuration to be used
- **API Endpoint**: `/api/truck-distance` is hardcoded in the program

### 7.2 API Request Format

The program sends data in the following JSON format:

```json
{
  "records": [
    {
      "VEHL_NO": "TN01AB1234",
      "GJAHR": "2025",
      "MONAT": "01",
      "TOTAL_DISTANCE": "1500.500",
      "DISTANCE_UNIT": "KM",
      "TRIP_COUNT": "25",
      "LIFNR": "1000001234"
    }
  ]
}
```

### 7.3 API Response Handling

- **Success (200 OK)**: Displays success message with response
- **Error**: Displays warning with status code and error message
- **No API URL**: Skips API call if URL is not configured in ZLOG_EXEC_VAR (test mode compatible)

### 7.4 Network Configuration

Ensure SAP system has network access to Fr8first platform:
- Configure HTTP proxy if required (SM59)
- Whitelist Fr8first URL in firewall
- Test connectivity using transaction SM59

### 7.5 Testing API Integration

1. Configure API URL in ZLOG_EXEC_VAR (NAME = 'SCM_API_URL')
2. Run program in test mode first (API call will be skipped in test mode)
3. Run program in normal mode
4. Check API response messages in program output
5. Verify data in Fr8first platform

---

## Step 8: Schedule Background Job (SM36) - Optional

For automated month-end processing:

### 8.1 Create Job

1. Go to SM36
2. Enter Job Name: ZSCM_TT_DIST_MONTHLY
3. Click "Start Condition"
4. Configure:
   - Date/Time: 1st of each month, 06:00 AM
   - Periodic Job: Yes
   - Period Values: Monthly
5. Save and go back

### 8.2 Define Step

1. Click "Step"
2. Click "ABAP Program"
3. Enter:
   - Program: ZSCM_MONTHLY_TT_DISTANCE
   - Variant: Create variant with default previous month
4. Save

### 8.3 Create Variant

1. In SE38, execute program ZSCM_MONTHLY_TT_DISTANCE
2. Enter selection screen values
3. Go to: Goto → Variants → Save as Variant
4. Enter:
   - Variant Name: Z_MONTHLY
   - Description: Monthly Background Execution
5. For P_MONAT and P_GJAHR, set as dynamic:
   - Selection Variable: D (Dynamic date calculation)
   - Name of Variable: Previous Month
6. Save variant

---

## Step 9: Execute Test Cases

Refer to: `ZSCM_MONTHLY_TT_DISTANCE_TestCases.md`

Execute all test cases and document results.

---

## Step 10: Transport to Quality/Production

### 10.1 Objects to Transport

| Object Type | Object Name | Description |
|-------------|-------------|-------------|
| DTEL | ZDIST_TOT | Data Element - Total Distance |
| DTEL | ZTRIP_CNT | Data Element - Trip Count |
| DTEL | ZDIST_SHIP | Data Element - Shipment Distance |
| TABL | ZSCM_MONTH_TT_DIST | Database Table (Header) |
| TABL | ZSCM_MONTH_TT_DET | Database Table (Detail/Child) |
| PROG | ZSCM_MONTHLY_TT_DISTANCE | ABAP Program |
| TRAN | ZTTDIST | Transaction Code (if created) |

### 10.2 Transport Steps

1. Go to SE09
2. Create/Select Workbench Request
3. Include all objects
4. Release tasks
5. Release request
6. Import in Quality system
7. Execute test cases in Quality
8. Import in Production system

### 10.3 Post-Transport Activities

1. **Quality System:**
   - Add ZLOG_EXEC_VAR configuration entries
   - Execute test cases
   - Sign off testing

2. **Production System:**
   - Add ZLOG_EXEC_VAR configuration entries
   - Schedule background job
   - Monitor first execution

---

## Troubleshooting

### Issue: No data in ALV output

**Possible Causes:**
1. No POD records in YTTSPOD for selected month with STATUS = 'C'
2. MFRGR not configured in ZLOG_EXEC_VAR
3. All MFRGR entries inactive (ACTIVE <> 'X')
4. Truck number (TRUCK_NO) blank in YTTSPOD
5. Billing documents not linked to deliveries in VBRP
6. Deliveries have no matching MFRGR in LIPS
7. Deliveries not linked to shipments in OIGSI

**Resolution:**
- Check YTTSPOD for records with RO_OUT_DATE in selected month and STATUS = 'C'
- Verify ZLOG_EXEC_VAR configuration for NAME = 'SCM_MONTH_TT_DIST'
- Check LIPS for deliveries with matching MFRGR (not VBRP - MFRGR is in LIPS)
- Check OIGSI for shipment-delivery assignments

### Issue: Records skipped (missing data)

**Possible Causes:**
1. Shipment has no distance in OIGSS (DISTZ = 0 or no record)
2. Not all deliveries of a shipment have POD with STATUS = 'C'
3. Some deliveries have RO_OUT_DATE outside selected month
4. Truck number not found for the billing document

**Resolution:**
- Check OIGSS for shipment distance records (SE16)
- Verify all deliveries of the shipment have completed POD status
- Check YTTSPOD data for all related billing documents
- Contact Transport team to update shipment distance data in OIGSS

---

## Support Information

| Area | Contact |
|------|---------|
| Development | SCM Development Team |
| Functional | SCM Transport Team |
| Basis | SAP Basis Team |

---

## Version History

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 1.0 | 2026-01-01 | SCM Dev Team | Initial version |
| 1.1 | 2026-01-01 | SCM Dev Team | Updated for TD Shipments, removed application logging |
| 1.2 | 2026-01-01 | SCM Dev Team | Route fetched directly from ZTRSTLMNT table |
| 1.3 | 2026-01-01 | SCM Dev Team | Route from OIGSV, Distance from TVRO |
| 1.4 | 2026-01-01 | SCM Dev Team | Variant name as constant (SCM_MONTH_TT_DIST) |
| 1.5 | 2026-01-01 | SCM Dev Team | Added child table ZSCM_MONTH_TT_DET for shipment details |
| 1.6 | 2026-01-01 | SCM Dev Team | Added REST API integration to send data to Fr8first platform |

