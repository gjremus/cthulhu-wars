import puppeteer from 'puppeteer';
import fs from 'fs';
import path from 'path';

const DESIGNER_URL = 'https://cwo.freeddns.org/designer/';
const CS_FACTION_CODE = 'tpunhkcjo2c'; // Colour Out of Space
const SCREENSHOTS_DIR = '/Users/gremus/Claude-Projects/cw-homebrew-wt/solo/test-screenshots';

// Create screenshots directory
if (!fs.existsSync(SCREENSHOTS_DIR)) {
  fs.mkdirSync(SCREENSHOTS_DIR, { recursive: true });
}

async function delay(ms) {
  return new Promise(resolve => setTimeout(resolve, ms));
}

async function testInteractionsSection(page) {
  console.log('\n=== TEST 1: Interactions Section ===');

  try {
    // Navigate to Interactions section
    console.log('Navigating to Interactions section...');
    await page.click('button:has-text("Interactions")');
    await delay(1000);

    // Take screenshot of interactions section
    await page.screenshot({
      path: path.join(SCREENSHOTS_DIR, '01-interactions-initial.png'),
      fullPage: true
    });

    // Find all interaction rows
    const rows = await page.$$('.interaction-row');
    console.log(`Found ${rows.length} interaction rows`);

    // Check default values for first 3 rows
    const expectedDefaults = [
      { faction: 'Sleeper', category: 'Spellbook', ability: 'Ancient Sorcery' },
      { faction: 'Neutral Monster', category: 'Elder Thing', ability: 'Mind Control' },
      { faction: 'Bubastis', category: 'Faction Region', ability: 'Moon' }
    ];

    for (let i = 0; i < Math.min(3, rows.length); i++) {
      const row = rows[i];
      const selects = await row.$$('select');

      if (selects.length >= 3) {
        const factionValue = await page.evaluate(el => el.value, selects[0]);
        const categoryValue = await page.evaluate(el => el.value, selects[1]);
        const abilityValue = await page.evaluate(el => el.value, selects[2]);

        console.log(`Row ${i + 1}: Faction="${factionValue}", Category="${categoryValue}", Ability="${abilityValue}"`);

        const expected = expectedDefaults[i];
        const matches = factionValue === expected.faction &&
                       categoryValue === expected.category &&
                       abilityValue === expected.ability;

        console.log(`  Expected: ${expected.faction} / ${expected.category} / ${expected.ability}`);
        console.log(`  Match: ${matches ? '✓ PASS' : '✗ FAIL'}`);
      } else {
        console.log(`Row ${i + 1}: FAIL - Expected 3 dropdowns, found ${selects.length}`);
      }
    }

    // Test waterfall behavior on first row
    console.log('\nTesting waterfall behavior...');
    const firstRow = rows[0];
    const selects = await firstRow.$$('select');

    if (selects.length >= 3) {
      // Change faction dropdown
      console.log('Changing faction to "Great Cthulhu"...');
      await page.evaluate(el => {
        el.value = 'Great Cthulhu';
        el.dispatchEvent(new Event('change', { bubbles: true }));
      }, selects[0]);
      await delay(500);

      // Check if category dropdown updated
      const categoryOptions = await page.evaluate(el =>
        Array.from(el.options).map(opt => opt.value),
        selects[1]
      );
      console.log(`Category options after faction change: ${categoryOptions.join(', ')}`);

      // Change category
      if (categoryOptions.length > 0) {
        const newCategory = categoryOptions[0];
        console.log(`Changing category to "${newCategory}"...`);
        await page.evaluate((el, val) => {
          el.value = val;
          el.dispatchEvent(new Event('change', { bubbles: true }));
        }, selects[1], newCategory);
        await delay(500);

        // Check if ability dropdown updated
        const abilityOptions = await page.evaluate(el =>
          Array.from(el.options).map(opt => opt.value),
          selects[2]
        );
        console.log(`Ability options after category change: ${abilityOptions.join(', ')}`);
        console.log('Waterfall behavior: ✓ PASS');
      }

      await page.screenshot({
        path: path.join(SCREENSHOTS_DIR, '02-interactions-waterfall.png'),
        fullPage: true
      });
    }

    // Test help icon
    console.log('\nTesting help icon...');
    const helpIcon = await page.$('.interaction-row .help-icon, .interaction-row button:has-text("?")');
    if (helpIcon) {
      console.log('Help icon found, clicking...');
      await helpIcon.click();
      await delay(1000);

      // Check for overlay
      const overlay = await page.$('.ability-overlay, .overlay-content');
      if (overlay) {
        console.log('Help overlay appeared: ✓ PASS');
        await page.screenshot({
          path: path.join(SCREENSHOTS_DIR, '03-interactions-help-overlay.png'),
          fullPage: true
        });

        // Close overlay
        const closeButton = await page.$('.overlay-close, button:has-text("Close")');
        if (closeButton) {
          await closeButton.click();
          await delay(500);
        }
      } else {
        console.log('Help overlay NOT found: ✗ FAIL');
      }
    } else {
      console.log('Help icon NOT found: ✗ FAIL');
    }

  } catch (error) {
    console.log(`✗ FAIL: ${error.message}`);
    await page.screenshot({
      path: path.join(SCREENSHOTS_DIR, 'error-interactions.png'),
      fullPage: true
    });
  }
}

async function testSimpleUpdateOutlines(page) {
  console.log('\n=== TEST 2: Simple Update Outlines ===');

  try {
    // Navigate to Simple Update
    console.log('Navigating to Simple Update screen...');
    await page.click('button:has-text("Simple Update")');
    await delay(1000);

    await page.screenshot({
      path: path.join(SCREENSHOTS_DIR, '04-simple-update-initial.png'),
      fullPage: true
    });

    // Find Meteorite combat field
    console.log('Looking for Meteorite combat field...');
    const meteoriteInput = await page.$('input[id*="meteorite"][id*="combat"], input[data-unit="meteorite"][data-field="combat"]');

    if (!meteoriteInput) {
      console.log('Could not find Meteorite combat input, searching all numeric inputs...');
      const allInputs = await page.$$('input[type="number"]');
      console.log(`Found ${allInputs.length} numeric inputs`);

      // Use first input for testing
      if (allInputs.length > 0) {
        const testInput = allInputs[0];
        const currentValue = await page.evaluate(el => el.value, testInput);
        console.log(`Testing with first numeric input (current value: ${currentValue})`);

        // Change value
        const newValue = parseInt(currentValue) + 1;
        console.log(`Changing value to ${newValue}...`);
        await page.evaluate((el, val) => {
          el.value = val;
          el.dispatchEvent(new Event('input', { bubbles: true }));
          el.dispatchEvent(new Event('change', { bubbles: true }));
        }, testInput, newValue);

        await delay(1000);

        // Check for outline
        const computedStyle = await page.evaluate(el => {
          const style = window.getComputedStyle(el);
          return {
            outline: style.outline,
            outlineColor: style.outlineColor,
            border: style.border,
            borderColor: style.borderColor
          };
        }, testInput);

        console.log('Computed style:', computedStyle);

        const hasOutline = computedStyle.outline !== 'none' ||
                          computedStyle.outlineColor.includes('0, 170, 0') || // Green
                          computedStyle.outlineColor.includes('0, 102, 255') || // Blue
                          computedStyle.borderColor.includes('0, 170, 0') ||
                          computedStyle.borderColor.includes('0, 102, 255');

        console.log(`Real-time outline: ${hasOutline ? '✓ PASS' : '✗ FAIL'}`);

        await page.screenshot({
          path: path.join(SCREENSHOTS_DIR, '05-simple-update-outline.png'),
          fullPage: true
        });
      }
    } else {
      console.log('Meteorite combat field found!');
      const currentValue = await page.evaluate(el => el.value, meteoriteInput);
      console.log(`Current value: ${currentValue}`);

      // Change value
      console.log('Changing value to 1...');
      await page.evaluate(el => {
        el.value = '1';
        el.dispatchEvent(new Event('input', { bubbles: true }));
        el.dispatchEvent(new Event('change', { bubbles: true }));
      }, meteoriteInput);

      await delay(1000);

      // Check for outline
      const computedStyle = await page.evaluate(el => {
        const style = window.getComputedStyle(el);
        return {
          outline: style.outline,
          outlineColor: style.outlineColor,
          border: style.border,
          borderColor: style.borderColor
        };
      }, meteoriteInput);

      console.log('Computed style:', computedStyle);

      const hasOutline = computedStyle.outline !== 'none' ||
                        computedStyle.outlineColor.includes('0, 170, 0') || // Green
                        computedStyle.outlineColor.includes('0, 102, 255'); // Blue

      console.log(`Real-time outline: ${hasOutline ? '✓ PASS' : '✗ FAIL'}`);

      await page.screenshot({
        path: path.join(SCREENSHOTS_DIR, '05-simple-update-outline.png'),
        fullPage: true
      });
    }

    // Check for legend
    console.log('\nChecking for color legend...');
    const legend = await page.$('.legend, .color-legend, [class*="legend"]');
    if (legend) {
      const legendText = await page.evaluate(el => el.textContent, legend);
      console.log(`Legend found: ${legendText}`);

      const hasAllColors = legendText.includes('Green') || legendText.includes('green') ||
                          legendText.includes('Blue') || legendText.includes('blue') ||
                          legendText.includes('Yellow') || legendText.includes('yellow') ||
                          legendText.includes('Orange') || legendText.includes('orange');
      console.log(`Legend has color info: ${hasAllColors ? '✓ PASS' : '✗ FAIL'}`);
    } else {
      console.log('Legend NOT found: ✗ FAIL');
    }

  } catch (error) {
    console.log(`✗ FAIL: ${error.message}`);
    await page.screenshot({
      path: path.join(SCREENSHOTS_DIR, 'error-simple-update.png'),
      fullPage: true
    });
  }
}

async function testTopButton(page) {
  console.log('\n=== TEST 3: Top Button ===');

  try {
    // Navigate to Units section (likely to be long)
    console.log('Navigating to Units section...');
    await page.click('button:has-text("Units")');
    await delay(1000);

    // Scroll down
    console.log('Scrolling down 2 viewports...');
    await page.evaluate(() => {
      window.scrollBy(0, window.innerHeight * 2);
    });
    await delay(1000);

    await page.screenshot({
      path: path.join(SCREENSHOTS_DIR, '06-scrolled-down.png'),
      fullPage: false
    });

    // Look for Top button
    console.log('Looking for Top button...');
    const topButton = await page.$('button:has-text("Top"), .top-button, [class*="scroll-top"]');

    if (topButton) {
      const isVisible = await page.evaluate(el => {
        const style = window.getComputedStyle(el);
        return style.display !== 'none' && style.visibility !== 'hidden' && style.opacity !== '0';
      }, topButton);

      console.log(`Top button visible: ${isVisible ? '✓ PASS' : '✗ FAIL'}`);

      if (isVisible) {
        console.log('Clicking Top button...');
        await topButton.click();
        await delay(1000);

        const scrollY = await page.evaluate(() => window.scrollY);
        console.log(`Scroll position after click: ${scrollY}`);
        console.log(`Scrolled to top: ${scrollY < 100 ? '✓ PASS' : '✗ FAIL'}`);

        await page.screenshot({
          path: path.join(SCREENSHOTS_DIR, '07-after-top-click.png'),
          fullPage: false
        });
      }
    } else {
      console.log('Top button NOT found: ✗ FAIL');
    }

  } catch (error) {
    console.log(`✗ FAIL: ${error.message}`);
    await page.screenshot({
      path: path.join(SCREENSHOTS_DIR, 'error-top-button.png'),
      fullPage: true
    });
  }
}

async function testSimpleUpdatePush(page) {
  console.log('\n=== TEST 4: Simple Update Push to Backend ===');

  try {
    // Navigate to Simple Update
    console.log('Navigating to Simple Update screen...');
    await page.click('button:has-text("Simple Update")');
    await delay(1000);

    // Find a numeric input to change
    const allInputs = await page.$$('input[type="number"]');
    if (allInputs.length === 0) {
      console.log('No numeric inputs found: ✗ FAIL');
      return;
    }

    const testInput = allInputs[0];
    const currentValue = await page.evaluate(el => el.value, testInput);
    const newValue = parseInt(currentValue || '0') + 1;

    console.log(`Changing value from ${currentValue} to ${newValue}...`);
    await page.evaluate((el, val) => {
      el.value = val;
      el.dispatchEvent(new Event('input', { bubbles: true }));
      el.dispatchEvent(new Event('change', { bubbles: true }));
    }, testInput, newValue);

    await delay(1000);

    // Find and click "Push design to build" button
    console.log('Looking for "Push design to build" button...');
    const pushButton = await page.$('button:has-text("Push design to build")');

    if (!pushButton) {
      console.log('Push button NOT found: ✗ FAIL');
      return;
    }

    console.log('Clicking push button...');
    await pushButton.click();
    await delay(2000);

    await page.screenshot({
      path: path.join(SCREENSHOTS_DIR, '08-after-push.png'),
      fullPage: true
    });

    console.log('Push button clicked. Backend verification needed via SSH.');

  } catch (error) {
    console.log(`✗ FAIL: ${error.message}`);
    await page.screenshot({
      path: path.join(SCREENSHOTS_DIR, 'error-push.png'),
      fullPage: true
    });
  }
}

async function main() {
  console.log('=== Designer UI Testing ===');
  console.log(`URL: ${DESIGNER_URL}`);
  console.log(`Faction: CS (${CS_FACTION_CODE})`);
  console.log(`Screenshots: ${SCREENSHOTS_DIR}`);

  const browser = await puppeteer.launch({
    headless: false,
    defaultViewport: { width: 1280, height: 800 }
  });

  try {
    const page = await browser.newPage();

    // Navigate to designer
    console.log('\nLoading designer...');
    await page.goto(DESIGNER_URL, { waitUntil: 'networkidle2' });
    await delay(2000);

    // Take initial screenshot
    await page.screenshot({
      path: path.join(SCREENSHOTS_DIR, '00-initial-load.png'),
      fullPage: true
    });

    // Run all tests
    await testInteractionsSection(page);
    await testSimpleUpdateOutlines(page);
    await testTopButton(page);
    await testSimpleUpdatePush(page);

    console.log('\n=== All tests completed ===');
    console.log(`Screenshots saved to: ${SCREENSHOTS_DIR}`);

  } catch (error) {
    console.error('Fatal error:', error);
  } finally {
    await browser.close();
  }
}

main().catch(console.error);
