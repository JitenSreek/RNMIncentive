# Functional Specification
# ZSCM_MONTHLY_TT_DISTANCE - Monthly Transport Truck Distance Calculation

| Document Information | |
|---------------------|---|
| **Document ID** | FS-SCM-TT-DIST-001 |
| **Version** | 2.0 |
| **Status** | Final |
| **Created Date** | 2026-01-02 |
| **Author** | SCM Development Team |
| **Module** | SCM - Supply Chain Management |
| **Sub-Module** | Transportation Management |

---

## 1. Document Control

### 1.1 Version History

| Version | Date | Author | Description |
|---------|------|--------|-------------|
| 1.0 | 2025-12-01 | SCM Team | Initial version using ZTRSTLMNT |
| 1.5 | 2025-12-15 | SCM Team | Added child table for shipment details |
| 1.6 | 2025-12-20 | SCM Team | Added REST API integration |
| 2.0 | 2026-01-02 | SCM Team | Refactored to OOP, new data sources (YTTSPOD, LIPS, OIGSS) |

### 1.2 Approvals

| Role | Name | Signature | Date |
|------|------|-----------|------|
| Business Owner | | | |
| Functional Lead | | | |
| Technical Lead | | | |
| Quality Assurance | | | |

---

## 2. Executive Summary

### 2.1 Purpose
This program calculates the cumulative monthly distance travelled by each Transport Truck (TT) based on completed shipment data. The calculated distance is stored in custom database tables and sent to an external platform (Fr8first) via REST API for further processing and incentive calculations.

### 2.2 Business Need
The organization needs to track the total distance covered by each transport truck on a monthly basis to:
- Calculate distance-based incentives for transporters
- Monitor vehicle utilization and efficiency
- Generate monthly distance reports for fleet management
- Integrate with external platforms for transporter payments

### 2.3 Scope
- **In Scope:**
  - Calculate monthly cumulative distance per vehicle
  - Store aggregated and detailed data in custom tables
  - Send data to external Fr8first platform via REST API
  - Display results in ALV grid format
  - Support test mode for validation without database updates

- **Out of Scope:**
  - Real-time distance calculation
  - Route optimization
  - Vehicle maintenance tracking
  - Driver management

---

## 3. Business Requirements

### 3.1 Functional Requirements

| Req ID | Requirement | Priority |
|--------|-------------|----------|
| FR-001 | Calculate total monthly distance per vehicle from completed shipments | High |
| FR-002 | Consider only shipments with POD STATUS = 'C' (Completed) | High |
| FR-003 | Filter shipments by Material Freight Group (MFRGR) from configuration | High |
| FR-004 | Validate all deliveries of each shipment meet completion criteria | High |
| FR-005 | Get shipment distance from OIGSS (Shipment Stage) table | High |
| FR-006 | Store aggregated data in header table (ZSCM_MONTH_TT_DIST) | High |
| FR-007 | Store shipment details in child table (ZSCM_MONTH_TT_DET) | High |
| FR-008 | Determine primary vendor (most frequent) for each vehicle | Medium |
| FR-009 | Send calculated data to Fr8first platform via REST API | High |
| FR-010 | Provide test mode to validate without database updates | Medium |
| FR-011 | Display results in ALV grid format | Medium |
| FR-012 | Support re-execution for same month (update existing records) | High |

### 3.2 Non-Functional Requirements

| Req ID | Requirement | Target |
|--------|-------------|--------|
| NFR-001 | Program execution time | < 5 minutes for 10,000 shipments |
| NFR-002 | Database transactions | Use COMMIT WORK for data integrity |
| NFR-003 | API response handling | Handle success and error responses |
| NFR-004 | Error messages | User-friendly error messages |
| NFR-005 | Compatibility | SAP ECC 6.0 / NetWeaver 7.31 |

---

## 4. Business Process Flow

### 4.1 Process Overview

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

### 4.2 Data Flow

| Step | Source | Target | Description |
|------|--------|--------|-------------|
| 1 | YTTSPOD | Internal Table | Get POD records for selected month with STATUS = 'C' |
| 2 | VBRP | Internal Table | Get delivery numbers for billing documents |
| 3 | LIPS | Internal Table | Get MFRGR and filter by configuration |
| 4 | OIGSI | Internal Table | Get shipment numbers for deliveries |
| 5 | OIGSI | Validation | Validate all shipment deliveries meet criteria |
| 6 | OIGSS | Internal Table | Get distance for each shipment |
| 7 | Processing | Aggregation | Aggregate distance by vehicle |
| 8 | Aggregation | ZSCM_MONTH_TT_DIST | Save header records |
| 9 | Aggregation | ZSCM_MONTH_TT_DET | Save detail records |
| 10 | Database | REST API | Send data to Fr8first platform |

---

## 5. Input Specifications

### 5.1 Selection Screen

| Block | Field | Type | Description | Mandatory |
|-------|-------|------|-------------|-----------|
| Selection Parameters | P_MONAT | Parameter | Month (01-12) | Yes |
| Selection Parameters | P_GJAHR | Parameter | Fiscal Year | Yes |
| Optional Filters | S_VEHL | Select-Option | Vehicle/Truck Number | No |
| Processing Options | P_TEST | Checkbox | Test Mode (no DB update) | No |

### 5.2 Configuration Tables

#### 5.2.1 ZLOG_EXEC_VAR - Material Freight Groups

| NAME | MFRGR | ACTIVE | Description |
|------|-------|--------|-------------|
| SCM_MONTH_TT_DIST | FG001 | X | Material Freight Group 1 |
| SCM_MONTH_TT_DIST | FG002 | X | Material Freight Group 2 |

#### 5.2.2 ZLOG_EXEC_VAR - API URL

| NAME | REMARKS | ACTIVE | Description |
|------|---------|--------|-------------|
| SCM_API_URL | http://api.fr8first.com | X | Fr8first API Base URL |

---

## 6. Processing Logic

### 6.1 Selection Criteria

**POD Records Selection (YTTSPOD):**
- RO_OUT_DATE >= First day of selected month
- RO_OUT_DATE <= Last day of selected month
- STATUS = 'C' (Completed)
- TRUCK_NO not blank
- TRUCK_NO in S_VEHL (if provided)

**Material Freight Group Filter (LIPS):**
- MFRGR must match entries in ZLOG_EXEC_VAR where NAME = 'SCM_MONTH_TT_DIST' and ACTIVE = 'X'

### 6.2 Validation Rules

| Rule | Description |
|------|-------------|
| VR-001 | All deliveries of a shipment must have POD with STATUS = 'C' |
| VR-002 | All deliveries of a shipment must have RO_OUT_DATE in selected month |
| VR-003 | Shipment must have distance record in OIGSS with DISTZ > 0 |
| VR-004 | Truck number must not be blank |

### 6.3 Calculation Logic

**Total Distance per Vehicle:**
```
TOTAL_DISTANCE = SUM(OIGSS-DISTZ) for all valid shipments of the vehicle
```

**Trip Count per Vehicle:**
```
TRIP_COUNT = COUNT(DISTINCT TKNUM) for all valid shipments of the vehicle
```

**Primary Vendor Determination:**
```
LIFNR = Vendor with highest shipment count for the vehicle in the month
```

---

## 7. Output Specifications

### 7.1 ALV Output

| Column | Description | Data Type |
|--------|-------------|-----------|
| VEHL_NO | Vehicle/Truck Number | CHAR 15 |
| GJAHR | Fiscal Year | NUMC 4 |
| MONAT | Month | NUMC 2 |
| TOTAL_DISTANCE | Total Distance Travelled | DEC 15,3 |
| DISTANCE_UNIT | Distance Unit (KM) | UNIT 3 |
| TRIP_COUNT | Number of Trips | INT4 |
| LIFNR | Primary Transporter Vendor | CHAR 10 |
| STATUS | Record Status (New/Updated/Test) | CHAR 10 |

### 7.2 Database Tables

#### 7.2.1 ZSCM_MONTH_TT_DIST (Header)

| Field | Description |
|-------|-------------|
| VEHL_NO | Vehicle Number (Key) |
| GJAHR | Year (Key) |
| MONAT | Month (Key) |
| TOTAL_DISTANCE | Aggregated Total Distance |
| DISTANCE_UNIT | Unit of Measure |
| TRIP_COUNT | Number of Trips |
| LIFNR | Primary Vendor |
| Audit Fields | Created/Changed By/On/Time |

#### 7.2.2 ZSCM_MONTH_TT_DET (Detail)

| Field | Description |
|-------|-------------|
| VEHL_NO | Vehicle Number (Key) |
| GJAHR | Year (Key) |
| MONAT | Month (Key) |
| TKNUM | Shipment Number (Key) |
| MFRGR | Material Freight Group |
| LIFNR | Vendor |
| RO_OUT_DATE | RO Out Date |
| DISTANCE | Shipment Distance |
| DISTANCE_UNIT | Unit of Measure |
| Audit Fields | Created By/On/Time |

### 7.3 REST API Output

**Endpoint:** `/api/truck-distance` (POST)

**Request Format:**
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

## 8. Error Handling

### 8.1 Error Messages

| Error Code | Message | Action |
|------------|---------|--------|
| E001 | Invalid month entered. Must be 01-12 | Correct input |
| E002 | Invalid year entered. Must be 1900-2099 | Correct input |
| E003 | No active Material Freight Groups found | Configure ZLOG_EXEC_VAR |
| E004 | No POD records found for selection | Check data or selection |
| E005 | Header table update failed | Contact support |
| E006 | Detail table insert failed | Contact support |
| E007 | API call failed | Check network/API configuration |

### 8.2 Warning Messages

| Warning | Message | Action |
|---------|---------|--------|
| W001 | No matching MFRGR found in deliveries | Check LIPS data |
| W002 | API URL not configured. API call skipped | Configure SCM_API_URL |
| W003 | X records skipped (missing data) | Check source data |

---

## 9. Security & Authorization

### 9.1 Authorization Objects

| Object | Field | Value | Description |
|--------|-------|-------|-------------|
| S_TCODE | TCD | ZTTDIST | Transaction Code |
| S_PROGRAM | P_GROUP | ZSCM | Program Group |

### 9.2 Data Security
- Program accesses only read-only data from source tables
- Write access limited to custom tables (ZSCM_MONTH_TT_*)
- API credentials stored in configuration table

---

## 10. Interfaces

### 10.1 Inbound Interfaces
None - Data sourced from SAP tables

### 10.2 Outbound Interfaces

| Interface | Target System | Protocol | Frequency |
|-----------|---------------|----------|-----------|
| Fr8first API | Fr8first Platform | REST/HTTP | On-demand (after DB save) |

---

## 11. Scheduling

### 11.1 Recommended Schedule
- **Frequency:** Monthly
- **Timing:** 1st of each month, 06:00 AM
- **Mode:** Background job with variant for previous month

### 11.2 Job Details
- **Job Name:** ZSCM_TT_DIST_MONTHLY
- **Program:** ZSCM_MONTHLY_TT_DISTANCE
- **Variant:** Dynamic date calculation for previous month

---

## 12. Assumptions & Dependencies

### 12.1 Assumptions
1. POD data in YTTSPOD is complete and accurate
2. MFRGR configuration in ZLOG_EXEC_VAR is maintained
3. Shipment distance in OIGSS is populated correctly
4. Fr8first API is available and accessible from SAP

### 12.2 Dependencies

| Dependency | Type | Description |
|------------|------|-------------|
| YTTSPOD | Table | POD data must be populated |
| VBRP | Table | Billing document items |
| LIPS | Table | Delivery items with MFRGR |
| OIGSI | Table | Shipment-delivery assignment |
| OIGSS | Table | Shipment distance data |
| ZLOG_EXEC_VAR | Config | MFRGR and API URL configuration |
| Fr8first API | External | API endpoint must be accessible |

---

## 13. Acceptance Criteria

| Criteria | Description | Status |
|----------|-------------|--------|
| AC-001 | Program calculates correct total distance per vehicle | |
| AC-002 | Only completed shipments (STATUS = 'C') are considered | |
| AC-003 | MFRGR filtering works correctly | |
| AC-004 | All deliveries validation works correctly | |
| AC-005 | Data saved correctly to header and detail tables | |
| AC-006 | Re-execution updates existing records correctly | |
| AC-007 | Test mode does not save to database | |
| AC-008 | API call successful with correct JSON format | |
| AC-009 | ALV output displays all required fields | |
| AC-010 | Error messages are clear and actionable | |

---

## 14. Glossary

| Term | Definition |
|------|------------|
| POD | Proof of Delivery |
| TT | Transport Truck |
| MFRGR | Material Freight Group |
| RO Out Date | Receipt Order Out Date |
| TD Shipment | Transportation/Dispatch Shipment |
| Fr8first | External transporter payment platform |

---

## Appendix A: Source Table Details

### YTTSPOD (Proof of Delivery)
| Field | Description |
|-------|-------------|
| BILLNO | Billing Document Number |
| RO_OUT_DATE | RO Out Date |
| STATUS | POD Status (C = Completed) |
| TRUCK_NO | Truck/Vehicle Number |
| MFRGR | Material Freight Group |

### VBRP (Billing Document Item)
| Field | Description |
|-------|-------------|
| VBELN | Billing Document |
| VGBEL | Reference Document (Delivery) |

### LIPS (Delivery Item)
| Field | Description |
|-------|-------------|
| VBELN | Delivery Number |
| MFRGR | Material Freight Group |

### OIGSI (Shipment-Delivery Assignment)
| Field | Description |
|-------|-------------|
| TKNUM | Shipment Number |
| VBELN | Delivery Number |

### OIGSS (Shipment Stage)
| Field | Description |
|-------|-------------|
| TKNUM | Shipment Number |
| DISTZ | Distance |
| MEDST | Unit of Measure |

---

*End of Functional Specification*

