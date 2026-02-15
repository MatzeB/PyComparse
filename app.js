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

  for (const row of results) {
    if (row.status === "ok") {
      ok += 1;
      cpythonMs += Number(row.cpython?.median_ms || 0);
      pycomparseMs += Number(row.pycomparse?.median_ms || 0);
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
  };
}

async function renderBenchmarks() {
  document.getElementById("bench-revision").textContent = BENCHMARK.revision;
  const status = document.getElementById("bench-status");
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
      label: "pycomparse revision",
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
  document.getElementById("bench-metrics").innerHTML = metricHtml;

  const rows = loaded
    .map(
      (s) => `
      <tr>
        <td>${s.name}</td>
        <td>${s.files.toLocaleString()}</td>
        <td>${fmtMs(s.cpythonMs)}</td>
        <td>${fmtMs(s.pycomparseMs)}</td>
        <td>${s.speedup === null ? "n/a" : `${s.speedup.toFixed(2)}x`}</td>
        <td>${s.notes}</td>
      </tr>`
    )
    .join("");

  document.getElementById("bench-table").innerHTML = rows;
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
    status.textContent =
      "Could not load README from GitHub. Use the link above to view it.";
    content.innerHTML = "";
  }
}

setupTabs();
renderBenchmarks();
loadReadme();
