const BENCHMARK = {
  revision: "08e5bf1",
  python: "3.8.20",
  suites: [
    {
      name: "CPython Lib suite",
      source: "bench_results/bench-all.json",
    },
    {
      name: "Synthetic large suite",
      source: "bench_results/bench-synthetic-large.json",
    },
  ],
};

const README_URL =
  "https://raw.githubusercontent.com/MatzeB/pycomparse/main/README.md";

const BENCH_SORT_KINDS = {
  suite: "text",
  files: "number",
  cpythonMs: "number",
  pycomparseMs: "number",
  speedup: "number",
  notes: "text",
};

const DETAIL_SORT_KINDS = {
  file: "text",
  sizeBytes: "number",
  cpythonMedian: "number",
  cpythonStddev: "number",
  pycomparseMedian: "number",
  pycomparseStddev: "number",
  speedup: "number",
  status: "text",
};

const benchSortState = { key: "suite", direction: "asc" };
const detailSortState = { key: "file", direction: "asc" };
let loadedSuites = [];

function selectTab(tab) {
  document.querySelectorAll(".tab").forEach((el) => {
    el.classList.toggle("is-active", el.dataset.tab === tab);
  });
  document.querySelectorAll(".panel").forEach((el) => {
    el.classList.toggle("is-active", el.id === `tab-${tab}`);
  });
}

function setupTabs() {
  document.querySelectorAll(".tab").forEach((btn) => {
    btn.addEventListener("click", () => {
      const tab = btn.dataset.tab;
      selectTab(tab);
      history.replaceState(null, "", `#${tab}`);
    });
  });

  const tabFromHash = location.hash.replace("#", "").trim();
  if (
    tabFromHash &&
    document.querySelector(`.tab[data-tab='${tabFromHash}']`)
  ) {
    selectTab(tabFromHash);
  }
}

function fmtMs(x) {
  return x.toLocaleString(undefined, {
    minimumFractionDigits: 2,
    maximumFractionDigits: 2,
  });
}

function fmtMaybeMs(x) {
  return Number.isFinite(x) ? fmtMs(x) : "n/a";
}

function fmtSpeedup(x) {
  return Number.isFinite(x) ? `${x.toFixed(2)}x` : "n/a";
}

function escapeHtml(text) {
  return String(text)
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/"/g, "&quot;")
    .replace(/'/g, "&#039;");
}

function getNestedNumber(obj, key) {
  if (!obj || typeof obj !== "object") {
    return NaN;
  }
  return Number(obj[key]);
}

function toggleSort(state, key) {
  if (state.key === key) {
    state.direction = state.direction === "asc" ? "desc" : "asc";
    return;
  }
  state.key = key;
  state.direction = "desc";
}

function compareByKind(a, b, kind) {
  if (kind === "number") {
    const av = Number.isFinite(a) ? a : Number.NEGATIVE_INFINITY;
    const bv = Number.isFinite(b) ? b : Number.NEGATIVE_INFINITY;
    return av - bv;
  }
  return String(a).localeCompare(String(b), undefined, {
    numeric: true,
    sensitivity: "base",
  });
}

function sortRows(rows, state, kindMap, valueFor) {
  const kind = kindMap[state.key] || "text";
  const direction = state.direction === "asc" ? 1 : -1;
  return [...rows].sort((a, b) => {
    const cmp = compareByKind(valueFor(a, state.key), valueFor(b, state.key), kind);
    if (cmp !== 0) {
      return cmp * direction;
    }
    return compareByKind(valueFor(a, "file"), valueFor(b, "file"), "text");
  });
}

function updateSortIndicators(tableId, state) {
  const table = document.getElementById(tableId);
  if (!table) {
    return;
  }
  table.querySelectorAll("th.sortable").forEach((th) => {
    th.classList.remove("is-sort-asc", "is-sort-desc");
    th.setAttribute("aria-sort", "none");
    if (th.dataset.sort === state.key) {
      th.classList.add(
        state.direction === "asc" ? "is-sort-asc" : "is-sort-desc"
      );
      th.setAttribute(
        "aria-sort",
        state.direction === "asc" ? "ascending" : "descending"
      );
    }
  });
}

function wireSortHeaders(tableId, state, rerender) {
  const table = document.getElementById(tableId);
  if (!table) {
    return;
  }
  table.querySelectorAll("th.sortable").forEach((th) => {
    th.tabIndex = 0;
    th.setAttribute("role", "button");
    th.onclick = () => {
      toggleSort(state, th.dataset.sort || "");
      rerender();
    };
    th.onkeydown = (event) => {
      if (event.key === "Enter" || event.key === " ") {
        event.preventDefault();
        toggleSort(state, th.dataset.sort || "");
        rerender();
      }
    };
  });
}

async function loadSuiteSummary(suite) {
  const res = await fetch(suite.source, { cache: "no-cache" });
  if (!res.ok) {
    throw new Error(`${suite.source}: HTTP ${res.status}`);
  }
  const payload = await res.json();
  const results = Array.isArray(payload.results) ? payload.results : [];

  let ok = 0;
  let fail = 0;
  let cpythonMs = 0;
  let pycomparseMs = 0;
  const details = [];

  for (const row of results) {
    const cpythonMedian = getNestedNumber(row.cpython, "median_ms");
    const pycomparseMedian = getNestedNumber(row.pycomparse, "median_ms");
    const cpythonStddev = getNestedNumber(row.cpython, "stddev_ms");
    const pycomparseStddev = getNestedNumber(row.pycomparse, "stddev_ms");
    let speedup = null;
    if (
      row.status === "ok" &&
      Number.isFinite(cpythonMedian) &&
      Number.isFinite(pycomparseMedian) &&
      pycomparseMedian > 0
    ) {
      speedup = cpythonMedian / pycomparseMedian;
    }

    const file = String(row.file || "");
    details.push({
      file,
      fileLower: file.toLowerCase(),
      sizeBytes: Number(row.size_bytes || 0),
      cpythonMedian,
      cpythonStddev,
      pycomparseMedian,
      pycomparseStddev,
      speedup,
      status: String(row.status || "unknown"),
    });

    if (row.status === "ok") {
      ok += 1;
      cpythonMs += Number((row.cpython && row.cpython.median_ms) || 0);
      pycomparseMs += Number((row.pycomparse && row.pycomparse.median_ms) || 0);
    } else {
      fail += 1;
    }
  }

  return {
    name: suite.name,
    file: suite.name,
    files: results.length,
    cpythonMs,
    pycomparseMs,
    speedup: pycomparseMs > 0 ? cpythonMs / pycomparseMs : null,
    notes: fail > 0 ? `${fail} failed` : "No failures",
    ok,
    fail,
    details,
  };
}

function summaryValue(row, key) {
  switch (key) {
    case "suite":
      return row.name;
    case "files":
      return row.files;
    case "cpythonMs":
      return row.cpythonMs;
    case "pycomparseMs":
      return row.pycomparseMs;
    case "speedup":
      return row.speedup;
    case "notes":
      return row.notes;
    case "file":
      return row.name;
    default:
      return row.name;
  }
}

function detailValue(row, key) {
  switch (key) {
    case "file":
      return row.file;
    case "sizeBytes":
      return row.sizeBytes;
    case "cpythonMedian":
      return row.cpythonMedian;
    case "cpythonStddev":
      return row.cpythonStddev;
    case "pycomparseMedian":
      return row.pycomparseMedian;
    case "pycomparseStddev":
      return row.pycomparseStddev;
    case "speedup":
      return row.speedup;
    case "status":
      return row.status;
    default:
      return row.file;
  }
}

function renderSummaryRows() {
  const tableEl = document.getElementById("bench-table");
  if (!tableEl) {
    return;
  }

  const sorted = sortRows(loadedSuites, benchSortState, BENCH_SORT_KINDS, summaryValue);
  const rows = sorted
    .map(
      (s) => `
      <tr>
        <td>${s.name}</td>
        <td>${s.files.toLocaleString()}</td>
        <td>${fmtMs(s.cpythonMs)}</td>
        <td>${fmtMs(s.pycomparseMs)}</td>
        <td>${fmtSpeedup(s.speedup)}</td>
        <td>${escapeHtml(s.notes)}</td>
      </tr>`
    )
    .join("");
  tableEl.innerHTML = rows;
  updateSortIndicators("bench-summary-table", benchSortState);
}

function renderDetailRows() {
  const suiteSelect = document.getElementById("detail-suite");
  const filterInput = document.getElementById("detail-filter");
  const detailStatus = document.getElementById("detail-status");
  const tbody = document.getElementById("detail-table");
  if (!suiteSelect || !filterInput || !detailStatus || !tbody) {
    return;
  }

  const suiteIndex = Number(suiteSelect.value || 0);
  const suite = loadedSuites[suiteIndex];
  if (!suite) {
    tbody.innerHTML = "";
    detailStatus.textContent = "No detailed benchmark data available.";
    return;
  }

  const filter = filterInput.value.trim().toLowerCase();
  const filteredRows = suite.details.filter((row) =>
    row.fileLower.includes(filter)
  );
  if (filteredRows.length === 0) {
    tbody.innerHTML =
      '<tr><td colspan="8">No files match the current filter.</td></tr>';
    detailStatus.textContent = `No matches in ${suite.name}.`;
    return;
  }

  const sortedRows = sortRows(
    filteredRows,
    detailSortState,
    DETAIL_SORT_KINDS,
    detailValue
  );

  const html = sortedRows
    .map((row) => {
      const file = escapeHtml(row.file);
      const rowClass = row.status === "ok" ? "" : ' class="row-failed"';
      return `
      <tr${rowClass}>
        <td title="${file}">${file}</td>
        <td>${row.sizeBytes.toLocaleString()}</td>
        <td>${fmtMaybeMs(row.cpythonMedian)}</td>
        <td>${fmtMaybeMs(row.cpythonStddev)}</td>
        <td>${fmtMaybeMs(row.pycomparseMedian)}</td>
        <td>${fmtMaybeMs(row.pycomparseStddev)}</td>
        <td>${fmtSpeedup(row.speedup)}</td>
        <td>${escapeHtml(row.status)}</td>
      </tr>`;
    })
    .join("");
  tbody.innerHTML = html;
  detailStatus.textContent =
    `Showing ${sortedRows.length.toLocaleString()} of ` +
    `${suite.details.length.toLocaleString()} files from ${suite.name}.`;
  updateSortIndicators("bench-detail-table", detailSortState);
}

function setupDetailControls() {
  const suiteSelect = document.getElementById("detail-suite");
  const filterInput = document.getElementById("detail-filter");
  if (!suiteSelect || !filterInput) {
    return;
  }

  const oldIndex = Number(suiteSelect.value || 0);
  suiteSelect.innerHTML = loadedSuites
    .map(
      (suite, index) =>
        `<option value="${index}">${escapeHtml(suite.name)}</option>`
    )
    .join("");
  suiteSelect.value = String(Math.min(oldIndex, Math.max(loadedSuites.length - 1, 0)));

  suiteSelect.onchange = () => renderDetailRows();
  filterInput.oninput = () => renderDetailRows();
  renderDetailRows();
}

function setupSortHandlers() {
  wireSortHeaders("bench-summary-table", benchSortState, renderSummaryRows);
  wireSortHeaders("bench-detail-table", detailSortState, renderDetailRows);
  updateSortIndicators("bench-summary-table", benchSortState);
  updateSortIndicators("bench-detail-table", detailSortState);
}

async function renderBenchmarks() {
  const status = document.getElementById("bench-status");
  const revisionEl = document.getElementById("bench-revision");
  const metricsEl = document.getElementById("bench-metrics");
  if (!status || !revisionEl || !metricsEl) {
    return;
  }

  revisionEl.textContent = BENCHMARK.revision;
  status.textContent = "Loading benchmark files...";

  const loaded = [];
  for (const suite of BENCHMARK.suites) {
    try {
      loaded.push(await loadSuiteSummary(suite));
    } catch (err) {
      loaded.push({
        name: suite.name,
        file: suite.name,
        files: 0,
        cpythonMs: 0,
        pycomparseMs: 0,
        speedup: null,
        notes: `Could not load: ${suite.source}`,
        ok: 0,
        fail: 0,
        details: [],
      });
    }
  }

  loadedSuites = loaded;
  const metrics = [
    {
      label: "Speedup",
      value: "5-10x",
    },
    {
      label: "Reference Python",
      value: `CPython ${BENCHMARK.python}`,
    },
    {
      label: "PyComparse revision",
      value: BENCHMARK.revision,
    },
  ];

  const metricHtml = metrics
    .map(
      (m) => `
        <article>
          <div>${m.label}</div>
          <div class="big">${m.value}</div>
        </article>`
    )
    .join("");
  metricsEl.innerHTML = metricHtml;

  renderSummaryRows();
  setupDetailControls();
  status.textContent = "Loaded from local bench_results JSON files.";
}

async function loadReadme() {
  const status = document.getElementById("readme-status");
  const content = document.getElementById("readme-content");

  try {
    const res = await fetch(README_URL, { cache: "no-cache" });
    if (!res.ok) {
      throw new Error(`HTTP ${res.status}`);
    }
    const markdown = await res.text();
    if (!window.marked) {
      throw new Error("markdown renderer not available");
    }
    content.innerHTML = window.marked.parse(markdown);
    status.textContent = "Loaded from GitHub main branch.";
  } catch (err) {
    status.innerHTML =
      "Could not load README from GitHub. " +
      '<a href="https://github.com/MatzeB/pycomparse/blob/main/README.md" ' +
      'target="_blank" rel="noreferrer">Open it on GitHub</a>.';
    content.innerHTML = "";
  }
}

setupTabs();
setupSortHandlers();

renderBenchmarks().catch((err) => {
  const status = document.getElementById("bench-status");
  const detailStatus = document.getElementById("detail-status");
  if (status) {
    status.textContent = `Benchmark render failed: ${String(err)}`;
  }
  if (detailStatus) {
    detailStatus.textContent = "Detailed results are unavailable.";
  }
});

loadReadme();
