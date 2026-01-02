# Function Specification: ZSCM_MONTHLY_TT_DISTANCE

| Document Information | |
|---------------------|---|
| **Document ID** | FS-SCM-TT-DIST-001 |
| **Version** | 2.0 |
| **Status** | Final |
| **Created Date** | 2026-01-02 |
| **Author** | SCM Development Team |
| **Module** | SCM - Supply Chain Management |

---

## Description

This program calculates the cumulative monthly distance travelled by each Transport Truck (TT) based on completed shipment data. The organization needs to track the total distance covered by each transport truck on a monthly basis to:
- Calculate distance-based incentives for transporters
- Monitor vehicle utilization and efficiency
- Generate monthly distance reports for fleet management
- Integrate with external Fr8first platform for transporter payments

The program retrieves shipment data from multiple SAP tables, validates completion status, aggregates distance per vehicle, stores results in custom database tables, and sends data to an external platform via REST API.

---

## Inputs

### Selection Screen Parameters

| Field Name | Type | Required | Constraints | Validation Rules |
|------------|------|----------|-------------|------------------|
| P_MONAT | NUMC(2) | Yes | 01-12 | Must be valid month (01-12) |
| P_GJAHR | NUMC(4) | Yes | 1900-2099 | Must be valid fiscal year |
| S_VEHL | CHAR(15) | No | Select-Option Range | Vehicle/Truck number filter |
| P_TEST | Boolean | No | Checkbox | Test mode - no database updates |

### Configuration Data (ZLOG_EXEC_VAR)

| Field Name | Type | Required | Constraints | Validation Rules |
|------------|------|----------|-------------|------------------|
| MFRGR Config | RANGE | Yes | NAME = 'SCM_MONTH_TT_DIST' | At least one active MFRGR entry required |
| API URL | STRING | Yes | NAME = 'SCM_API_URL' | Valid HTTP/HTTPS URL in REMARKS field |

### Source Data Tables

| Table | Fields Used | Description |
|-------|-------------|-------------|
| YTTSPOD | BILLNO, RO_OUT_DATE, STATUS, TRUCK_NO, MFRGR | Proof of Delivery records |
| VBRP | VBELN, VGBEL | Billing document items (delivery reference) |
| LIPS | VBELN, MFRGR | Delivery item (Material Freight Group) |
| OIGSI | TKNUM, VBELN | Shipment-Delivery assignment |
| OIGSS | TKNUM, DISTZ, MEDST | Shipment stage (distance data) |

---

## Output

### ALV Display Output

| Field Name | Type | Description |
|------------|------|-------------|
| VEHL_NO | CHAR(15) | Vehicle/Truck Number |
| GJAHR | NUMC(4) | Fiscal Year |
| MONAT | NUMC(2) | Month |
| TOTAL_DISTANCE | DEC(15,3) | Total Distance Travelled in the month |
| DISTANCE_UNIT | UNIT(3) | Distance Unit (KM) |
| TRIP_COUNT | INT4 | Number of completed trips/shipments |
| LIFNR | CHAR(10) | Primary Transporter Vendor (most frequent) |
| STATUS | CHAR(10) | Processing Status (New/Updated/Test Mode) |

### Database Output - Header Table (ZSCM_MONTH_TT_DIST)

| Field Name | Type | Description |
|------------|------|-------------|
| MANDT | CLNT(3) | Client |
| VEHL_NO | CHAR(15) | Vehicle Number (Key) |
| GJAHR | NUMC(4) | Year (Key) |
| MONAT | NUMC(2) | Month (Key) |
| TOTAL_DISTANCE | DEC(15,3) | Aggregated Total Distance |
| DISTANCE_UNIT | UNIT(3) | Unit of Measure |
| TRIP_COUNT | INT4 | Number of Trips |
| LIFNR | CHAR(10) | Primary Vendor |
| CREATED_BY | CHAR(12) | Created By User |
| CREATED_ON | DATS(8) | Created Date |
| CREATED_TM | TIMS(6) | Created Time |
| CHANGED_BY | CHAR(12) | Changed By User |
| CHANGED_ON | DATS(8) | Changed Date |
| CHANGED_TM | TIMS(6) | Changed Time |

### Database Output - Detail Table (ZSCM_MONTH_TT_DET)

| Field Name | Type | Description |
|------------|------|-------------|
| MANDT | CLNT(3) | Client |
| VEHL_NO | CHAR(15) | Vehicle Number (Key) |
| GJAHR | NUMC(4) | Year (Key) |
| MONAT | NUMC(2) | Month (Key) |
| TKNUM | CHAR(10) | Shipment Number (Key) |
| MFRGR | CHAR(8) | Material Freight Group |
| LIFNR | CHAR(10) | Transporter Vendor |
| RO_OUT_DATE | DATS(8) | RO Out Date |
| ROUTE | CHAR(6) | Route Code (legacy field) |
| DISTANCE | DEC(15,3) | Shipment Distance |
| DISTANCE_UNIT | UNIT(3) | Distance Unit |
| CREATED_BY | CHAR(12) | Created By User |
| CREATED_ON | DATS(8) | Created Date |
| CREATED_TM | TIMS(6) | Created Time |

### REST API Output (Fr8first Platform)

| Field Name | Type | Description |
|------------|------|-------------|
| records | ARRAY | Array of vehicle distance records |
| records[].VEHL_NO | STRING | Vehicle Number |
| records[].GJAHR | STRING | Fiscal Year |
| records[].MONAT | STRING | Month |
| records[].TOTAL_DISTANCE | STRING | Total Distance |
| records[].DISTANCE_UNIT | STRING | Distance Unit |
| records[].TRIP_COUNT | STRING | Number of Trips |
| records[].LIFNR | STRING | Primary Vendor |

**API Request Format:**
```json
{
  "records": [
    {
      "VEHL_NO": "MH12AB1234",
      "GJAHR": "2025",
      "MONAT": "12",
      "TOTAL_DISTANCE": "1500.500",
      "DISTANCE_UNIT": "KM",
      "TRIP_COUNT": "25",
      "LIFNR": "1000001234"
    }
  ]
}
```

---

## Behavior

1. **Validate Input Parameters**: Verify P_MONAT is between 01-12 and P_GJAHR is between 1900-2099. Display error message and stop processing if validation fails.

2. **Calculate Date Range**: Determine first day and last day of the selected month using function module `HR_JP_MONTH_BEGIN_END_DATE`.

3. **Fetch Material Freight Group Configuration**: Read ZLOG_EXEC_VAR table where NAME = 'SCM_MONTH_TT_DIST' and ACTIVE = 'X'. Build range table of valid MFRGR values. If no active entries found, display error and stop processing.

4. **Fetch API URL Configuration**: Read ZLOG_EXEC_VAR table where NAME = 'SCM_API_URL' and ACTIVE = 'X'. Extract API base URL from REMARKS field. If not found, display warning (API call will be skipped).

5. **Select POD Records from YTTSPOD**:
   - Filter: RO_OUT_DATE between first and last day of selected month
   - Filter: STATUS = 'C' (Completed)
   - Filter: TRUCK_NO not blank
   - Filter: TRUCK_NO in S_VEHL (if selection-option provided)
   - Extract: BILLNO, RO_OUT_DATE, TRUCK_NO, MFRGR

6. **Get Deliveries from VBRP**:
   - For each unique BILLNO from step 5
   - Select VBELN (billing doc) and VGBEL (delivery reference) from VBRP
   - Filter: VGBEL is not blank
   - Use FOR ALL ENTRIES for performance

7. **Get Material Freight Group from LIPS**:
   - For each unique delivery (VGBEL) from step 6
   - Select VBELN (delivery) and MFRGR from LIPS
   - Filter: MFRGR in configured range (from step 3)
   - Filter: MFRGR is not blank
   - Filter deliveries from step 6 to keep only those with matching MFRGR

8. **Get Shipments from OIGSI**:
   - For each valid delivery from step 7
   - Select TKNUM (shipment) and VBELN (delivery) from OIGSI
   - Use FOR ALL ENTRIES for performance

9. **Validate All Deliveries of Each Shipment**:
   - For each shipment from step 8, retrieve ALL deliveries assigned to that shipment
   - Check that EVERY delivery has a corresponding POD record with:
     - STATUS = 'C'
     - RO_OUT_DATE within selected month
   - Exclude shipments where any delivery fails this validation

10. **Get Distance from OIGSS**:
    - For each valid shipment from step 9
    - Select TKNUM, DISTZ (distance), MEDST (unit) from OIGSS
    - Take first record per shipment
    - Filter: DISTZ > 0 (must have valid distance)

11. **Build Shipment Data**:
    - Combine data from all sources into consolidated shipment records
    - Include: TKNUM, TRUCK_NO, MFRGR, LIFNR, RO_OUT_DATE, DISTANCE, DIST_UNIT
    - Skip records with missing truck number, MFRGR, or distance
    - Remove duplicate shipments

12. **Aggregate Distance per Vehicle**:
    - Group shipments by TRUCK_NO (vehicle)
    - Calculate: TOTAL_DISTANCE = SUM(DISTANCE) for all shipments
    - Calculate: TRIP_COUNT = COUNT(DISTINCT TKNUM)
    - Determine: Primary LIFNR = Vendor with highest shipment count for the vehicle

13. **Prepare Output Records**:
    - For each vehicle, create header record (ZSCM_MONTH_TT_DIST format)
    - For each shipment, create detail record (ZSCM_MONTH_TT_DET format)
    - Set status: 'New' for new records, 'Updated' for existing records

14. **Save to Database** (if not Test Mode):
    - Check for existing header records in ZSCM_MONTH_TT_DIST
    - DELETE existing detail records from ZSCM_MONTH_TT_DET for the month
    - MODIFY header records (insert or update)
    - INSERT detail records
    - Execute COMMIT WORK AND WAIT
    - On failure, execute ROLLBACK WORK

15. **Send Data to REST API** (if not Test Mode and API URL configured):
    - Retrieve saved header records from ZSCM_MONTH_TT_DIST
    - Convert to JSON format using /UI2/CL_JSON
    - Create HTTP client using CL_HTTP_CLIENT=>CREATE_BY_URL
    - Set method to POST, content-type to application/json
    - Send request and handle response

16. **Display ALV Output**:
    - Create ALV grid using CL_SALV_TABLE
    - Display aggregated results with all columns
    - Show processing summary (records created, updated, skipped)

---

## Errors

| Error Code | Error Message | Trigger Condition |
|------------|---------------|-------------------|
| E-MONTH-01 | Invalid month entered. Must be 01-12. | P_MONAT < 01 or P_MONAT > 12 |
| E-YEAR-01 | Invalid year entered. Must be 1900-2099. | P_GJAHR < 1900 or P_GJAHR > 2099 |
| E-CONFIG-01 | No active Material Freight Groups found in configuration | ZLOG_EXEC_VAR has no active entries for 'SCM_MONTH_TT_DIST' |
| E-DATA-01 | No POD records found for selection criteria | YTTSPOD returns no records |
| E-DATA-02 | No matching deliveries found in VBRP | VBRP returns no records for billing documents |
| E-DATA-03 | No deliveries with matching MFRGR found in LIPS | LIPS returns no records with configured MFRGR |
| E-DATA-04 | No shipments found for matching deliveries | OIGSI returns no shipment records |
| E-DATA-05 | No shipments have valid distance data | OIGSS returns no distance records |
| E-DB-01 | Failed to update header table | MODIFY ZSCM_MONTH_TT_DIST fails (sy-subrc <> 0) |
| E-DB-02 | Failed to insert detail records | INSERT ZSCM_MONTH_TT_DET fails (sy-subrc <> 0) |
| E-API-01 | REST API call failed | HTTP request fails or returns error status |
| W-CONFIG-01 | API URL not configured. API call skipped. | ZLOG_EXEC_VAR has no entry for 'SCM_API_URL' |
| W-DATA-01 | X records skipped due to missing data | Shipments without truck number, MFRGR, or distance |

---

## Success

- All validated shipments are correctly aggregated by vehicle
- TOTAL_DISTANCE equals sum of individual shipment distances
- TRIP_COUNT equals count of distinct shipments
- Primary vendor correctly identified (most frequent vendor for the vehicle)
- Header records saved to ZSCM_MONTH_TT_DIST with correct status (New/Updated)
- Detail records saved to ZSCM_MONTH_TT_DET for audit trail
- REST API receives valid JSON payload
- REST API returns HTTP 200 status
- ALV grid displays all processed records with correct values
- Summary displays counts: New records, Updated records, Skipped records
- Response structure:

```
Processing Summary:
  - Records Created: X
  - Records Updated: Y
  - Records Skipped: Z
  - Detail Records: N
  - API Call: Success/Failed/Skipped
```

---

## Edge Cases

- **No POD records for selected month**: Display informational message "No POD records found for selection criteria" and exit gracefully. ALV shows empty grid.

- **Shipment has multiple deliveries with different completion dates**: All deliveries must have RO_OUT_DATE within selected month AND STATUS = 'C'. If any delivery fails, entire shipment is excluded.

- **Shipment has no distance in OIGSS**: Skip the shipment and increment skipped counter. Log in summary.

- **Same shipment appears in multiple billing documents**: Deduplicate by TKNUM before aggregation to avoid double-counting distance.

- **Vehicle has shipments from multiple vendors**: Determine primary vendor by frequency (most shipments). If tie, use first vendor alphabetically.

- **Re-execution for same month**: Delete existing detail records for the month, then MODIFY header records (update if exists, insert if new). Maintains data integrity.

- **Test mode execution**: Process all data normally but skip database save and API call. Set STATUS = 'Test Mode' for all output records.

- **API URL not configured**: Skip API call but continue with database save. Display warning message.

- **API call fails**: Log error but do not rollback database changes (data is already saved). Display warning in summary.

- **Empty vehicle filter (S_VEHL)**: Process all vehicles without filter restriction.

- **Zero distance in OIGSS**: Skip shipments with DISTZ = 0 or blank. These are invalid distance records.

- **Blank truck number in YTTSPOD**: Skip POD records where TRUCK_NO is blank.

---

## Dependencies

### External Services
| Service | Purpose | Protocol |
|---------|---------|----------|
| Fr8first API | Send calculated distance data | REST/HTTP POST |

### SAP Tables (Read)
| Table | Purpose |
|-------|---------|
| YTTSPOD | Proof of Delivery records |
| VBRP | Billing document items |
| LIPS | Delivery items |
| OIGSI | Shipment-delivery assignment |
| OIGSS | Shipment stage (distance) |
| ZLOG_EXEC_VAR | Configuration (MFRGR, API URL) |

### SAP Tables (Write)
| Table | Purpose |
|-------|---------|
| ZSCM_MONTH_TT_DIST | Store aggregated monthly distance |
| ZSCM_MONTH_TT_DET | Store shipment details |

### SAP Function Modules
| Function Module | Purpose |
|-----------------|---------|
| HR_JP_MONTH_BEGIN_END_DATE | Calculate first/last day of month |

### SAP Classes
| Class | Purpose |
|-------|---------|
| CL_HTTP_CLIENT | HTTP communication for API calls |
| /UI2/CL_JSON | JSON serialization |
| CL_SALV_TABLE | ALV grid display |

### Events
- None (batch program, no event triggers)

---

## Performance

| Metric | Requirement | Notes |
|--------|-------------|-------|
| Max Latency | < 5 minutes | For processing 10,000 shipments |
| Database Reads | Optimized | FOR ALL ENTRIES with empty checks |
| Internal Processing | Optimized | SORT + BINARY SEARCH for lookups |
| API Call | < 30 seconds | Single bulk POST request |
| Memory | Efficient | Type-specific structures, no SELECT * |

### Optimization Techniques Applied
- FOR ALL ENTRIES with IS NOT INITIAL check before each SELECT
- BINARY SEARCH after SORT for all READ TABLE operations
- SELECT only required fields (no SELECT *)
- DELETE ADJACENT DUPLICATES after SORT for deduplication
- Single database transaction with COMMIT WORK AND WAIT
- Bulk API call instead of individual calls per record

---

## Security

| Aspect | Requirement |
|--------|-------------|
| Authentication | Standard SAP user authentication required |
| Authorization | Transaction code authorization (S_TCODE) |
| PII Masking | Not applicable (no PII data) |
| Data Encryption | HTTPS for API communication |
| Audit Trail | CREATED_BY, CREATED_ON, CHANGED_BY, CHANGED_ON in tables |
| Access Control | Read access to source tables, Write access to custom tables only |

### Authorization Objects
| Object | Field | Value | Description |
|--------|-------|-------|-------------|
| S_TCODE | TCD | ZTTDIST | Transaction Code |
| S_PROGRAM | P_GROUP | ZSCM | Program Group |

---

## Tests

### Happy Path Tests

| Test ID | Description | Expected Result |
|---------|-------------|-----------------|
| AT-001 | Execute for month with valid POD data, matching MFRGR, valid distances | Records saved to both tables, API called successfully, ALV shows results |
| AT-002 | Execute for month with multiple vehicles | Each vehicle aggregated separately with correct totals |
| AT-003 | Execute with vehicle filter (S_VEHL) | Only filtered vehicles processed |
| AT-004 | Re-execute for same month | Existing records updated, no duplicates |

### Error Path Tests

| Test ID | Description | Expected Result |
|---------|-------------|-----------------|
| AT-005 | Execute with invalid month (13) | Error message E-MONTH-01, processing stops |
| AT-006 | Execute with invalid year (2100) | Error message E-YEAR-01, processing stops |
| AT-007 | Execute with no MFRGR configuration | Error message E-CONFIG-01, processing stops |
| AT-008 | Execute for month with no POD records | Informational message, empty ALV |
| AT-009 | Execute when LIPS has no matching MFRGR | Warning message, no data processed |
| AT-010 | Execute when API URL not configured | Database saved, warning for API skip |
| AT-011 | Execute when API call fails | Database saved, error logged for API |

### Edge Case Tests

| Test ID | Description | Expected Result |
|---------|-------------|-----------------|
| AT-012 | Shipment with multiple deliveries, all completed | Shipment included in calculation |
| AT-013 | Shipment with multiple deliveries, one incomplete | Shipment excluded from calculation |
| AT-014 | Shipment with zero distance | Shipment skipped, counter incremented |
| AT-015 | Vehicle with multiple vendors | Primary vendor determined by frequency |
| AT-016 | Execute in Test Mode | No database changes, STATUS = 'Test Mode' |
| AT-017 | Duplicate shipment across billing documents | Counted once in aggregation |
| AT-018 | Blank truck number in POD | Record skipped |

### Performance Tests

| Test ID | Description | Expected Result |
|---------|-------------|-----------------|
| AT-019 | Execute with 1,000 shipments | Complete within 30 seconds |
| AT-020 | Execute with 10,000 shipments | Complete within 5 minutes |
| AT-021 | Execute with 100 vehicles, 50 shipments each | Correct aggregation, reasonable time |

---

## Appendix A: Process Flow Diagram

```
┌─────────────────────────────────────────────────────────────────────┐
│                    MONTHLY DISTANCE CALCULATION                      │
├─────────────────────────────────────────────────────────────────────┤
│                                                                      │
│  ┌──────────┐    ┌──────────┐    ┌──────────┐    ┌──────────┐      │
│  │   POD    │───▶│ Billing  │───▶│ Delivery │───▶│ Shipment │      │
│  │ (YTTSPOD)│    │  (VBRP)  │    │  (LIPS)  │    │ (OIGSI)  │      │
│  └──────────┘    └──────────┘    └──────────┘    └──────────┘      │
│       │                               │               │             │
│       │                               │               │             │
│       ▼                               ▼               ▼             │
│  ┌──────────────────────────────────────────────────────────┐      │
│  │              VALIDATION & FILTERING                       │      │
│  │  • STATUS = 'C'                                          │      │
│  │  • RO_OUT_DATE in selected month                         │      │
│  │  • MFRGR in configuration                                │      │
│  │  • All deliveries of shipment must meet criteria         │      │
│  └──────────────────────────────────────────────────────────┘      │
│                              │                                      │
│                              ▼                                      │
│                    ┌──────────────┐                                │
│                    │    OIGSS     │                                │
│                    │  (Distance)  │                                │
│                    └──────────────┘                                │
│                              │                                      │
│                              ▼                                      │
│                    ┌──────────────┐                                │
│                    │  Aggregate   │                                │
│                    │  by Vehicle  │                                │
│                    └──────────────┘                                │
│                              │                                      │
│              ┌───────────────┼───────────────┐                     │
│              ▼               ▼               ▼                     │
│       ┌──────────┐    ┌──────────┐    ┌──────────┐                │
│       │  Header  │    │  Detail  │    │ REST API │                │
│       │  Table   │    │  Table   │    │ Fr8first │                │
│       └──────────┘    └──────────┘    └──────────┘                │
│                                                                     │
└─────────────────────────────────────────────────────────────────────┘
```

---

## Appendix B: Glossary

| Term | Definition |
|------|------------|
| POD | Proof of Delivery - Document confirming goods delivery |
| TT | Transport Truck - Vehicle used for transportation |
| MFRGR | Material Freight Group - Categorization of materials for freight |
| RO Out Date | Receipt Order Out Date - Date when goods left origin |
| TD Shipment | Transportation/Dispatch Shipment |
| Fr8first | External transporter payment and tracking platform |
| LIFNR | Vendor Number - SAP vendor master identifier |
| TKNUM | Shipment Number - SAP shipment document identifier |

---

## Appendix C: Configuration Setup

### ZLOG_EXEC_VAR - Material Freight Groups

| NAME | MFRGR | ACTIVE | REMARKS |
|------|-------|--------|---------|
| SCM_MONTH_TT_DIST | FG001 | X | Fuel Products |
| SCM_MONTH_TT_DIST | FG002 | X | Lubricants |
| SCM_MONTH_TT_DIST | FG003 | X | LPG |

### ZLOG_EXEC_VAR - API URL

| NAME | MFRGR | ACTIVE | REMARKS |
|------|-------|--------|---------|
| SCM_API_URL | | X | https://api.fr8first.com |

---

*End of Function Specification*
