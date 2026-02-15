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
  if (tabFromHash && document.querySelector(`.tab[data-tab='${tabFromHash}']`)) {
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
    let speedup = Number(row.ratio_cpython_over_pycomparse);
    if (!Number.isFinite(speedup) && pycomparseMedian > 0) {
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

function renderDetailRows(loaded) {
  const suiteSelect = document.getElementById("detail-suite");
  const filterInput = document.getElementById("detail-filter");
  const detailStatus = document.getElementById("detail-status");
  const tbody = document.getElementById("detail-table");
  if (!suiteSelect || !filterInput || !detailStatus || !tbody) {
    return;
  }

  const suiteIndex = Number(suiteSelect.value || 0);
  const suite = loaded[suiteIndex];
  if (!suite) {
    tbody.innerHTML = "";
    detailStatus.textContent = "No detailed benchmark data available.";
    return;
  }

  const filter = filterInput.value.trim().toLowerCase();
  const rows = suite.details.filter((row) => row.fileLower.includes(filter));
  if (rows.length === 0) {
    tbody.innerHTML =
      '<tr><td colspan="8">No files match the current filter.</td></tr>';
    detailStatus.textContent = `No matches in ${suite.name}.`;
    return;
  }

  const html = rows
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
    `Showing ${rows.length.toLocaleString()} of ` +
    `${suite.details.length.toLocaleString()} files from ${suite.name}.`;
}

function setupDetailControls(loaded) {
  const suiteSelect = document.getElementById("detail-suite");
  const filterInput = document.getElementById("detail-filter");
  if (!suiteSelect || !filterInput) {
    return;
  }

  suiteSelect.innerHTML = loaded
    .map(
      (suite, index) =>
        `<option value="${index}">${escapeHtml(suite.name)}</option>`
    )
    .join("");

  suiteSelect.onchange = () => renderDetailRows(loaded);
  filterInput.oninput = () => renderDetailRows(loaded);
  renderDetailRows(loaded);
}

async function renderBenchmarks() {
  const status = document.getElementById("bench-status");
  const revisionEl = document.getElementById("bench-revision");
  const tableEl = document.getElementById("bench-table");
  const metricsEl = document.getElementById("bench-metrics");
  if (!status || !revisionEl || !tableEl || !metricsEl) {
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

  const speedups = loaded.map((s) => s.speedup).filter((x) => x !== null);
  const best = speedups.length ? Math.max(...speedups) : null;
  const metrics = [
    {
      label: "Best speedup",
      value: best === null ? "n/a" : `${best.toFixed(2)}x`,
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

  const rows = loaded
    .map(
      (s) => `
      <tr>
        <td>${s.name}</td>
        <td>${s.files.toLocaleString()}</td>
        <td>${fmtMs(s.cpythonMs)}</td>
        <td>${fmtMs(s.pycomparseMs)}</td>
        <td>${fmtSpeedup(s.speedup)}</td>
        <td>${s.notes}</td>
      </tr>`
    )
    .join("");

  tableEl.innerHTML = rows;
  setupDetailControls(loaded);
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
      'Could not load README from GitHub. ' +
      '<a href="https://github.com/MatzeB/pycomparse/blob/main/README.md" ' +
      'target="_blank" rel="noreferrer">Open it on GitHub</a>.';
    content.innerHTML = "";
  }
}

setupTabs();

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
