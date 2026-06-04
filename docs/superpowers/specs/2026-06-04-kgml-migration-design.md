# Migrate `docs/index.html` to HHP KGML v2 — Design

_Date: 2026-06-04_
_Source handoff: `/Users/zachary.hoylman/temp/handoff/HHP_KGML_STREAMFLOW_HANDOFF.md`_

## Goal

Switch the Headwaters Hydrology Project map (`docs/index.html`, served at
`streamflow.climate.umt.edu`) off the old single-file `current_flow_percentile.geojson`
product and onto the new HHP KGML v2 two-file product (static geometry basemap + daily
percentile record set, joined on `huc10`).

## Decisions (locked)

| Decision | Choice |
|---|---|
| Display percentile | **Hybrid** — empirical normally, stagge when `q_mm_d <= 0`, fall back to whichever is non-null |
| Basemap format | **TopoJSON** (`huc10-basemap.topojson`), adds `topojson-client` dependency |
| USGS toggle | **Keep as-is** (separate data source, unaffected by migration) |
| Folded-in cleanups | **Yes** — close broken `@media` block, remove duplicate/dead script |
| Base path | **Ship the test path now**, isolated as a single `BASE` const for one-line promotion |

## Architecture

Unchanged shape: one static `docs/index.html`, no build step. Only the data layer changes.
The map, legend, colors, opacity slider, states overlay, sidebar, and USGS toggle all stay.
Add one `<script>` for `topojson-client` (only new dependency) and one config const.

```
const BASE = "https://data.climate.umt.edu/share/hhp_v2_test/hhp-kgml-test";
```

## Data flow

```
Promise.all([
  fetch(`${BASE}/huc10-basemap.topojson`),                  // 15,363 geoms, props {huc10, name}
  fetch(`${BASE}/huc10-current-percentiles_latest.json`),   // { target_date, records[huc10] }
])
→ topojson.feature(topo, topo.objects.huc10_conus_simplified)   // → GeoJSON FeatureCollection
→ for each feature:
    huc10 = f.properties.huc10                  // string; leading zeros significant
    rec   = records[huc10]
    if (!rec || rec.fit_status === "too_few") drop
    merge rec into f.properties
    f.properties.pctile  = hybridPercentile(rec)   // 0–1 or null
    f.properties.plotUrl = `${BASE}/plots/huc10/huc4=${huc10.slice(0,4)}/huc10-${huc10}.png`
→ L.geoJson(filtered, { style, onEachFeature }).addTo(map)
→ geojsonLoaded = true; maybe hide spinner
```

### Hybrid percentile (§5 of handoff)

```js
function hybridPercentile(rec) {
  const q = rec.q_mm_d;
  let src = (q != null && q <= 0) ? rec.stagge_percentile : rec.empirical_percentile;
  if (src == null || isNaN(src)) {
    src = (src === rec.stagge_percentile) ? rec.empirical_percentile : rec.stagge_percentile;
  }
  return (src == null || isNaN(src)) ? null : src / 100;   // 0–1, or null → grey
}
```

## Field remapping

| Old | New |
|---|---|
| `feature.properties.basin_id` | `feature.properties.huc10` (string) |
| `feature.properties.basin_name` | `feature.properties.name` |
| `feature.properties.ft3_s_ecdf` (0–1) | derived `feature.properties.pctile` (0–1) |
| plot `current_climatology_plot_${basin_id}.png` (flat) | `${BASE}/plots/huc10/huc4=${huc10.slice(0,4)}/huc10-${huc10}.png` |
| `current_flow_percentile_time.txt` fetch | use `records.target_date` from the percentiles JSON |

## Styling & `fit_status` policy (§7)

- `ok` / `all_zero` → colored by `pctile` on the existing 11-band USDM scale (legend unchanged).
- `fit_failed` → rendered **grey** (`pctile` is null → grey fill).
- `too_few` → dropped entirely (no geometry shown, no plot exists).

Color scale is identical to the current app (no legend change):

```
breaks  = [0, 0.02, 0.05, 0.1, 0.2, 0.3, 0.7, 0.8, 0.9, 0.95, 0.98, 1]
colors  = ["#730000","#E60000","#FFAA00","#FCD37F","#FFFF00","#FFFFFF",
           "#82FCF9","#32E1FA","#325CFE","#4030E3","#303B83"]
```

The `style()` function must handle `pctile === null` by returning a grey fill instead of
running it through the chroma scale.

## Closed-basin lakes — deliberate behavior change

The old app excluded ~12 lakes (Great Salt Lake, Pyramid Lake, etc.) by matching
`basin_name`. Those names came from the old product; KGML uses HUC10 sub-watershed names
that will not match that list, and KGML's `fit_status` is the intended mechanism for
dry/unfittable basins.

**Decision:** drop the name-based `excludedBasins` filter; rely on `fit_status`.
**Risk:** a few lake HUC10s may render colored. Flag for visual QA once live; add a
HUC10-based skip list later if any look wrong.

## Tooltip & sidebar

- Tooltip: `name` + ordinal percentile derived from `pctile` (reuse existing ordinal logic).
  For `fit_failed` (null `pctile`) show "no reliable fit" instead of a percentile.
- Sidebar title: `Basin ID (HUC10): <huc10>`.
- Sidebar image: `plotUrl + "?cacheBust=" + Date.now()`, with an `img.onerror` handler that
  replaces the image with a "No plot available for this basin" message.

## Time / floating title

Replace the separate `current_flow_percentile_time.txt` fetch with `records.target_date`
from the percentiles JSON. Floating title: `Current Streamflow Percentiles (<target_date>)`;
`document.title` likewise.

## Error handling

- `img.onerror` on sidebar plot → "No plot available for this basin".
- Join misses and `too_few` basins are silently dropped (expected).
- Loading spinner still gated on `baseMapLoaded && geojsonLoaded`; `geojsonLoaded` flips
  true after the join + layer add. A fetch/parse failure logs to console and leaves the
  spinner logic as-is (matches current failure behavior).

## Folded-in cleanups

1. Close the unclosed `@media (max-width: 768px)` block (add the missing `}`).
2. Remove the redundant duplicate sidebar-resizer IIFE and the dead `UsgsToggleControl`
   definition in the first IIFE. Keep the working sidebar drag, a single resizer, and the
   live USGS toggle (the IIFE that actually calls `map.addControl(...)`). Verify no shared
   references (`geoJsonLayer`, `currentOpacity`, `setHhpOpacity`, etc.) break.

## Testing / verification

No test framework exists (static page, live data deps). Verification:

1. `curl` the three endpoints to confirm the live structure matches the handoff:
   - `huc10-basemap.topojson` → object `huc10_conus_simplified`, props `{huc10, name}`
   - `huc10-current-percentiles_latest.json` → `target_date`, `records[huc10]` with the
     documented fields
   - one plot URL resolves (200) and one `too_few` basin 404s
2. Load `docs/index.html` in a browser and confirm: basins load and color correctly,
   hover tooltips, click → plot in sidebar, opacity slider, USGS toggle still works, mobile
   bottom-sheet layout. Screenshot for confirmation.

## Out of scope

- No change to the USGS overlay data or behavior.
- No change to the states overlay, legend bins/colors, or general layout.
- No unrelated refactoring beyond the two folded-in cleanups.
