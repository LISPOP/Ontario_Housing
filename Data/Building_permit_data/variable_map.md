# Building Permit Variable Map

Source: `Data/Building_permit_data/variable_map.xlsx`

Each cell gives the name of the field in that municipality's file. A dash (—) means the variable is not in that municipality's file.

| Variable | Plain-language summary | Toronto | Ottawa | Mississauga | Hamilton | Kitchener | St. Catharines | Burlington |
|---|---|---|---|---|---|---|---|---|
| Permit Type | The kind of permit or application (e.g., building, demolition, plumbing). | `PERMIT_TYPE` | `APPL. TYPE` | `SCOPE` | `WORKCLASS` | `Permit Type` | `FOLDERDESC` | `FOLDERTYPE` |
| Structure Type | The kind of building the permit covers (e.g., single detached house, townhouse, apartment, commercial). | `STRUCTURE_TYPE` | `BLG TYPE` | `BLDG_TYPE` | `PERMITCLAS` | `Sub Work Type` | — | `SUBDESC` |
| Work | A description of the work being done (e.g., new building, addition, interior alteration). | `WORK` | `DESCRIPTION` | `DESCRIPTIO` | `DESCRIPTIO` | `Work Type` | `FOLDERDE_1` | `WORKDESC` |
| Issued Date | The date the city issued the permit. | `ISSUED_DATE` | `ISSUED DATE` | `ISSUE_DATE` | `ISSUEDDATE` | `Issue Date` | `ISSUEDATE` | `ISSUEDATE` |
| Completed Date | The date the work was finished and the permit was closed. | `COMPLETED_DATE` | — | `COMPLETE_D` | `COMPLETEDD` | `Final Date` | — | — |
| Street Address | Where the property is. | `STREET_NUM`; `STREET_NAME`; `STREET_TYPE` | `ST #`; `ROAD` | `ADDRESS` | `ORIGINALAD` | `Foldername` | `PROPERTYNA` | `ADDRESS` |
| Current Use | How the building or property is used before the work. | `CURRENT_USE` | — | — | — | — | — | — |
| Proposed Use | How the building or property will be used after the work. | `PROPOSED_USE` | — | — | — | `Permit Description` | `FOLDERDE_1` | `SUBDESC` |
| Postal Code | The property's postal code. | `POSTAL` | `PC` | `POSTAL_COD` | `ORIGINALZI` | — | `PROPPOSTAL` | — |
| Dwelling Units Created | The number of new homes (residential units) the project adds. | `DWELLING_UNITS_CREATED` | `D.U.` | `RES_UNITS` | — | `Units Created` | — | `RESUNITS` |
| Status | Where the permit stands (e.g., issued, under inspection, closed, cancelled). | `STATUS` | — | `STATUS` | `STATUSCURR` | `Permit Status` | `STATUSDESC` | `FOLDERSTAT` |

## Notes

- **Kitchener:** X/Y coordinates are also available (from the spreadsheet's Notes column).
- **St. Catharines:** `FOLDERDE_1` is mapped to both Work and Proposed Use.
- **Burlington:** `SUBDESC` is mapped to both Structure Type and Proposed Use.
- **Toronto** and **Ottawa** split the street address across several fields.
