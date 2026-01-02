# Test Cases: ZSCM_MONTHLY_TT_DISTANCE

## Overview
This document contains test cases for the Monthly Transport Truck Distance Calculation program.

**Version 2.0 Changes:**
- Data source changed from ZTRSTLMNT to YTTSPOD → VBRP → OIGSI
- Distance retrieved from OIGSS (instead of TVRO Route Master)
- Validation: All deliveries of each shipment must have STATUS = 'C' and RO_OUT_DATE in selected month

---

## Pre-requisites

1. Table `ZSCM_MONTH_TT_DIST` created and activated in SE11
2. Table `ZSCM_MONTH_TT_DET` created and activated in SE11
3. Program `ZSCM_MONTHLY_TT_DISTANCE` created and activated in SE38
4. Configuration entries exist in `ZLOG_EXEC_VAR` with:
   - NAME = 'SCM_MONTH_TT_DIST' with valid MFRGR values (ACTIVE = 'X')
   - NAME = 'SCM_API_URL' with API URL in REMARKS field (optional)
5. Test data available:
   - YTTSPOD: POD records with STATUS = 'C' and RO_OUT_DATE in test month
   - VBRP: Billing items with MFRGR matching configuration
   - OIGSI: Shipment-delivery assignments
   - OIGSS: Shipment distance records

---

## Test Case Execution

### AT-001: Happy Path - Valid Shipments Processing

| Attribute | Value |
|-----------|-------|
| Test ID | AT-001 |
| Description | Run for a month with valid shipments |
| Pre-condition | POD records exist in YTTSPOD with STATUS='C', linked to deliveries in VBRP with matching MFRGR, shipments in OIGSI, and distances in OIGSS |

**Steps:**
1. Execute program ZSCM_MONTHLY_TT_DISTANCE (SE38 or SA38)
2. Enter Month: 12, Year: 2025
3. Leave Test Mode unchecked
4. Execute

**Expected Result:**
- ALV displays list of vehicles with calculated distances
- Records saved to ZSCM_MONTH_TT_DIST (header)
- Records saved to ZSCM_MONTH_TT_DET (detail)
- Status column shows 'New' for first run
- Success message displayed

**Verification SQL:**
```sql
SELECT * FROM zscm_month_tt_dist
WHERE gjahr = '2025' AND monat = '12';

SELECT * FROM zscm_month_tt_det
WHERE gjahr = '2025' AND monat = '12';
```

---

### AT-002: Invalid Month Validation

| Attribute | Value |
|-----------|-------|
| Test ID | AT-002 |
| Description | Run with invalid month (13) |
| Pre-condition | None |

**Steps:**
1. Execute program ZSCM_MONTHLY_TT_DISTANCE
2. Enter Month: 13, Year: 2025
3. Try to execute

**Expected Result:**
- Error message E001: "Invalid month entered. Must be 01-12."
- Program does not execute

---

### AT-003: Missing Configuration

| Attribute | Value |
|-----------|-------|
| Test ID | AT-003 |
| Description | Run with no configuration in ZLOG_EXEC_VAR |
| Pre-condition | No active entries exist for NAME = 'SCM_MONTH_TT_DIST' in ZLOG_EXEC_VAR |

**Steps:**
1. Delete or deactivate all entries in ZLOG_EXEC_VAR where NAME = 'SCM_MONTH_TT_DIST'
2. Execute program ZSCM_MONTHLY_TT_DISTANCE
3. Enter Month: 12, Year: 2025
4. Execute

**Expected Result:**
- Error message: "No active Material Freight Groups found for variant SCM_MONTH_TT_DIST"
- Program stops execution

---

### AT-004: No Active MFRGR

| Attribute | Value |
|-----------|-------|
| Test ID | AT-004 |
| Description | Run with all MFRGR entries inactive |
| Pre-condition | All entries for NAME = 'SCM_MONTH_TT_DIST' have ACTIVE = '' (blank) |

**Steps:**
1. Deactivate all MFRGR entries in ZLOG_EXEC_VAR where NAME = 'SCM_MONTH_TT_DIST'
2. Execute program ZSCM_MONTHLY_TT_DISTANCE
3. Enter Month: 12, Year: 2025
4. Execute

**Expected Result:**
- Error message: "No active Material Freight Groups found for variant SCM_MONTH_TT_DIST"
- Program stops execution

---

### AT-005: Test Mode Execution

| Attribute | Value |
|-----------|-------|
| Test ID | AT-005 |
| Description | Run in test mode - no database update |
| Pre-condition | Valid shipments exist with complete data flow |

**Steps:**
1. Execute program ZSCM_MONTHLY_TT_DISTANCE
2. Enter valid Month: 12, Year: 2025
3. Check 'Test Mode' checkbox
4. Execute

**Expected Result:**
- ALV displays calculated results
- Status column shows 'Test Mode' for all records
- No records inserted/updated in ZSCM_MONTH_TT_DIST or ZSCM_MONTH_TT_DET
- Message: "Test Mode: X vehicles processed. No data saved."
- API call skipped in test mode

**Verification:**
- Check ZSCM_MONTH_TT_DIST - no new records for the month
- Check ZSCM_MONTH_TT_DET - no new records for the month

---

### AT-006: Re-run for Same Month (Update)

| Attribute | Value |
|-----------|-------|
| Test ID | AT-006 |
| Description | Re-run for same month - existing records updated |
| Pre-condition | AT-001 executed successfully, records exist |

**Steps:**
1. Execute program ZSCM_MONTHLY_TT_DISTANCE
2. Enter same Month: 12, Year: 2025 as AT-001
3. Leave Test Mode unchecked
4. Execute

**Expected Result:**
- ALV displays results
- Status column shows 'Updated' for existing records
- CHANGED_BY, CHANGED_ON, CHANGED_TM fields updated in header table
- CREATED_BY, CREATED_ON, CREATED_TM fields unchanged in header table
- Detail records (ZSCM_MONTH_TT_DET) replaced (deleted and reinserted)
- No duplicate records created

**Verification SQL:**
```sql
SELECT vehl_no, gjahr, monat, COUNT(*)
FROM zscm_month_tt_dist
WHERE gjahr = '2025' AND monat = '12'
GROUP BY vehl_no, gjahr, monat
HAVING COUNT(*) > 1;
```
Result should be empty (no duplicates).

---

### AT-007: Missing Distance in OIGSS

| Attribute | Value |
|-----------|-------|
| Test ID | AT-007 |
| Description | Shipment with no distance record in OIGSS |
| Pre-condition | At least one shipment has no record in OIGSS or DISTZ = 0 |

**Steps:**
1. Identify a shipment in OIGSI that has no corresponding record in OIGSS
2. Execute program with selection including this shipment
3. Execute

**Expected Result:**
- Record skipped, not included in output
- Skip count displayed in summary message: "X records skipped (missing data)"
- Other valid records processed normally

---

### AT-008: Background Job Execution

| Attribute | Value |
|-----------|-------|
| Test ID | AT-008 |
| Description | Run as background job |
| Pre-condition | Valid shipments exist |

**Steps:**
1. Execute program ZSCM_MONTHLY_TT_DISTANCE
2. Enter valid parameters
3. Press F9 (Execute in Background) or use SM36
4. Schedule job immediately
5. Monitor job in SM37

**Expected Result:**
- Job completes successfully (check SM37 for job status)
- Records saved to ZSCM_MONTH_TT_DIST
- Verify data in custom table using SE16

---

### AT-009: Vehicle Filter

| Attribute | Value |
|-----------|-------|
| Test ID | AT-009 |
| Description | Run with S_VEHL filter for specific vehicles |
| Pre-condition | Multiple vehicles exist with shipments |

**Steps:**
1. Execute program ZSCM_MONTHLY_TT_DISTANCE
2. Enter valid Month, Year, Variant
3. Enter specific vehicle number in S_VEHL filter
4. Execute

**Expected Result:**
- Only the selected vehicle(s) processed
- ALV shows only filtered vehicles
- Database records created only for selected vehicles

---

### AT-010: No Shipments Found

| Attribute | Value |
|-----------|-------|
| Test ID | AT-010 |
| Description | Run for month with no shipments |
| Pre-condition | No shipments exist for the selected month/MFRGR combination |

**Steps:**
1. Execute program for a month with no data (e.g., future month)
2. Execute

**Expected Result:**
- Warning message W001: "No shipments found for the selection criteria"
- Empty ALV or no display
- No database changes

---

## Performance Test

| Attribute | Value |
|-----------|-------|
| Test ID | AT-PERF-001 |
| Description | Performance test with large data volume |
| Pre-condition | 10,000+ shipments in test month |

**Steps:**
1. Execute program with large data set
2. Monitor execution time

**Expected Result:**
- Execution completes within reasonable time (< 5 minutes for 10K records)
- No memory dumps
- FOR ALL ENTRIES optimization working correctly

---

## Test Sign-Off

| Test ID | Status | Tester | Date | Comments |
|---------|--------|--------|------|----------|
| AT-001 | | | | |
| AT-002 | | | | |
| AT-003 | | | | |
| AT-004 | | | | |
| AT-005 | | | | |
| AT-006 | | | | |
| AT-007 | | | | |
| AT-008 | | | | |
| AT-009 | | | | |
| AT-010 | | | | |
| AT-PERF-001 | | | | |

---

## Additional Setup Requirements

### Create Text Elements (SE38)

Maintain the following text elements for the program:
- TEXT-001: Selection Parameters
- TEXT-002: Optional Filters
- TEXT-003: Processing Options

### Create Message Class (Optional)

If custom message class needed:
- Message Class: ZSCM_TT
- Create messages for E001-E004, W001-W003, I001

