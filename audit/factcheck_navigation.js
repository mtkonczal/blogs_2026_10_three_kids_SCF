/* Shared navigation for the three independently authored review panels. */
(() => {
  const panels = [...document.querySelectorAll('[role="tabpanel"]')];
  const bar = document.querySelector('[role="tablist"]');
  if (document.getElementById('cross-check') && !document.getElementById('tab-cross')) {
    const button = document.createElement('button');
    button.id = 'tab-cross'; button.type = 'button'; button.role = 'tab';
    button.textContent = 'Cross-Fact Check'; button.tabIndex = -1;
    button.setAttribute('aria-controls', 'cross-check');
    button.setAttribute('aria-selected', 'false'); bar.append(button);
  }
  if (document.getElementById('deepseek') && !document.getElementById('tab-deepseek')) {
    const button = document.createElement('button');
    button.id = 'tab-deepseek'; button.type = 'button'; button.role = 'tab';
    button.textContent = 'DeepSeek'; button.tabIndex = -1;
    button.setAttribute('aria-controls', 'deepseek');
    button.setAttribute('aria-selected', 'false');
    const cross = document.getElementById('tab-cross');
    if (cross) bar.insertBefore(button, cross); else bar.append(button);
  }
  const tabs = [...bar.querySelectorAll('[role="tab"]')];
  const wide = matchMedia('(min-width:1000px)');
  const rails = new Map();
  function disclose(target) {
    let ancestor = target.parentElement;
    while (ancestor) { if (ancestor.tagName === 'DETAILS') ancestor.open = true; ancestor = ancestor.parentElement; }
  }
  function activate(panel, focus = false) {
    panels.forEach(p => { p.hidden = p !== panel; });
    tabs.forEach(t => {
      const selected = t.getAttribute('aria-controls') === panel.id;
      t.setAttribute('aria-selected', String(selected)); t.tabIndex = selected ? 0 : -1;
      if (selected && focus) t.focus();
    });
    updateCurrent();
  }
  function navigate(id, history = true) {
    const target = document.getElementById(id);
    if (!target) return;
    const panel = target.closest('[role="tabpanel"]');
    if (!panel) return;
    activate(panel); disclose(target);
    if (!wide.matches) rails.get(panel)?.removeAttribute('open');
    if (history) window.history.pushState(null, '', '#' + id);
    target.focus({preventScroll: true}); target.scrollIntoView({block: 'start'});
    updateCurrent();
  }
  function wrapHeading(heading) {
    let block = heading;
    const parent = heading.parentElement;
    if ((parent.matches('section,.x-note') && !parent.matches('[role="tabpanel"]')) || parent.matches('.final-recommendations')) block = parent;
    const disclosure = document.createElement('details'); disclosure.className = 'review-section';
    const summary = document.createElement('summary');
    const content = document.createElement('div'); content.className = 'section-content';
    block.before(disclosure); disclosure.append(summary, content);
    if (block !== heading) {
      summary.append(heading); content.append(block);
    } else {
      let next = heading.nextElementSibling;
      summary.append(heading);
      while (next && !next.matches('h2[id],h3[id],details.review-section,.x-note,.handoff,.weakest-points,.codex-cross-container')) {
        const following = next.nextElementSibling; content.append(next); next = following;
      }
    }
    return disclosure;
  }
  panels.forEach(panel => {
    const oldLinks = panel.id === 'codex' ? [...panel.querySelectorAll('.contents-links a')] : [...panel.querySelectorAll('.c2-rail a')];
    const headingIds = [...new Set(oldLinks.map(a => a.hash.slice(1)))];
    panel.querySelectorAll('.contents-rail,.c2-rail,.c2-mobile').forEach(node => node.remove());
    const headings = headingIds.map(id => document.getElementById(id)).filter(h => h?.matches('h2,h3') && h.id !== 'x-codex');
    panel.querySelectorAll('.codex-cross-review h2[id]').forEach(h => { if (!headings.includes(h)) headings.push(h); });
    // Work backwards so each section stops at the next disclosure boundary.
    [...headings].reverse().forEach(wrapHeading);
    const disclosures = [...panel.querySelectorAll('details.review-section')];
    disclosures.forEach((d, i) => { d.open = i === 0 || d.querySelector('#cx-verdict,#cx-final') !== null; });
    const aside = document.createElement('aside'); aside.className = 'review-rail';
    const rail = document.createElement('details'); rail.open = wide.matches;
    const label = document.createElement('summary'); label.textContent = 'On this page'; rail.append(label);
    const nav = document.createElement('nav'); nav.setAttribute('aria-label', panel.id + ' review sections'); rail.append(nav); aside.append(rail); panel.prepend(aside); rails.set(panel, rail);
    // A group is [label, child ids, optional id the label itself links to].
    // Review panels expose three main parts; supporting audit sections appear
    // under the executive summary only while reading that part (see updateCurrent).
    const claudeTail = ['c2-weakest', 'c2-rebuttals'];
    const groups = panel.id === 'cross-check' ? [
      ['Claude’s cross-check', headingIds.filter(id => id !== 'x-codex' && id !== 'ds-crosscheck')],
      ['Codex’s cross-check', ['x-codex', ...headings.filter(h => h.id.startsWith('cx-') && h.id !== 'cx-final').map(h => h.id)]],
      ['Final recommendations', ['cx-final']],
      ['DeepSeek’s note', ['ds-crosscheck']]
    ] : panel.id === 'codex' ? [
      ['Executive Summary', headingIds.filter(id => !['overall-assessment', 'weakest-points', 'easiest-rebuttals'].includes(id)), 'overall-assessment'],
      ['Three Weakest Points', [], 'weakest-points'],
      ['Five Substantive Objections', [], 'easiest-rebuttals']
    ] : panel.id === 'other-ai' ? [
      ['Executive summary', headingIds.filter(id => id !== 'c2-summary' && !claudeTail.includes(id)), 'c2-summary'],
      ['Three weakest points', [], 'c2-weakest'],
      ['Five easiest rebuttals', [], 'c2-rebuttals']
    ] : panel.id === 'deepseek' ? [
      ['Executive Summary', headingIds.filter(id => id !== 'ds-summary' && id !== 'ds-weakest' && id !== 'ds-rebuttals'), 'ds-summary'],
      ['Three Weakest Points', [], 'ds-weakest'],
      ['Five Easiest Rebuttals', [], 'ds-rebuttals']
    ] : [[panel.id === 'codex' ? 'Codex review' : 'Review', headingIds]];
    groups.forEach(([name, ids, headId]) => {
      // A group whose sections are all absent is dropped rather than left as dead text.
      const present = ids.filter(id => document.getElementById(id));
      if (!present.length && !(headId && document.getElementById(headId))) return;
      ids = present;
      const group = document.createElement('details'); group.open = true; group.className = 'rail-group';
      const title = document.createElement('summary');
      if (headId && document.getElementById(headId)) {
        const head = document.createElement('a'); head.href = `#${headId}`; head.textContent = name;
        title.append(head);
        // The text navigates; the rest of the summary still toggles the disclosure.
        title.addEventListener('click', event => {
          if (!event.target.closest('a')) return;
          event.preventDefault(); event.stopPropagation(); navigate(headId);
        });
      } else {
        title.textContent = name;
      }
      group.append(title);
      if (!ids.length) { group.classList.add('rail-leaf'); nav.append(group); return; }
      if (['codex', 'other-ai', 'deepseek'].includes(panel.id)) group.dataset.autoclose = '1';
      const list = document.createElement('ol');
      ids.forEach(id => {
        const target = document.getElementById(id); if (!target) return;
        const item = document.createElement('li'); const link = document.createElement('a');
        link.href = `#${id}`;
        link.textContent = id === 'x-start' ? 'Claude: start' : id === 'x-end' ? 'Claude: end' : id === 'x-codex' ? 'Codex: start' : target.textContent;
        item.append(link); list.append(item);
      });
      group.append(list); nav.append(group);
    });
    const controls = document.createElement('div'); controls.className = 'section-controls';
    [['Expand all sections', true], ['Collapse all sections', false]].forEach(([name, open]) => {
      const button = document.createElement('button'); button.type = 'button'; button.textContent = name;
      button.addEventListener('click', () => { disclosures.forEach(d => { d.open = open; }); }); controls.append(button);
    });
    const body = panel.querySelector('#review,.revbody'); body.prepend(controls);
    panel.addEventListener('toggle', scheduleCurrent, true);
  });
  tabs.forEach((tab, i) => {
    tab.addEventListener('click', () => activate(document.getElementById(tab.getAttribute('aria-controls'))));
    tab.addEventListener('keydown', event => {
      let next;
      if (event.key === 'ArrowRight') next = (i + 1) % tabs.length;
      else if (event.key === 'ArrowLeft') next = (i + tabs.length - 1) % tabs.length;
      else if (event.key === 'Home') next = 0;
      else if (event.key === 'End') next = tabs.length - 1;
      else return;
      event.preventDefault(); activate(document.getElementById(tabs[next].getAttribute('aria-controls')), true);
    });
  });
  document.addEventListener('click', event => {
    const a = event.target.closest('a[href^="#"]');
    if (!a || event.button !== 0 || event.metaKey || event.ctrlKey || event.shiftKey || event.altKey) return;
    if (!document.getElementById(a.hash.slice(1))?.closest('[role="tabpanel"]')) return;
    event.preventDefault(); navigate(a.hash.slice(1));
  });
  let pending = false;
  let openGroup;
  function updateCurrent() {
    pending = false;
    const panel = panels.find(p => !p.hidden); if (!panel) return;
    const links = [...panel.querySelectorAll('.review-rail a')];
    let current = links[0];
    links.forEach(a => { const t = document.getElementById(a.hash.slice(1)); if (t.getBoundingClientRect().top <= 150) current = a; });
    links.forEach(a => { if (a === current) a.setAttribute('aria-current', 'location'); else a.removeAttribute('aria-current'); });
    // Groups marked autoclose hold their sections open only while the reader is inside
    // them. Re-applied only when the current group changes, so a manual toggle sticks.
    const group = current?.closest('.rail-group') ?? null;
    if (group !== openGroup) {
      openGroup = group;
      panel.querySelectorAll('.rail-group[data-autoclose]').forEach(g => { g.open = g === group; });
    }
  }
  function scheduleCurrent() { if (!pending) { pending = true; requestAnimationFrame(updateCurrent); } }
  window.addEventListener('scroll', scheduleCurrent, {passive: true});
  window.addEventListener('resize', scheduleCurrent);
  window.addEventListener('hashchange', () => navigate(decodeURIComponent(location.hash.slice(1)), false));
  wide.addEventListener('change', () => { rails.forEach(rail => { rail.open = wide.matches; }); });
  let printState;
  window.addEventListener('beforeprint', () => {
    printState = [...document.querySelectorAll('details.review-section')].map(d => [d, d.open]);
    printState.forEach(([d]) => { d.open = true; });
  });
  window.addEventListener('afterprint', () => { printState?.forEach(([d, open]) => { d.open = open; }); });
  if (location.hash) navigate(decodeURIComponent(location.hash.slice(1)), false); else updateCurrent();
})();
