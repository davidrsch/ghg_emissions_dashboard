/**
 * Function: ComboBoxquery
 *
 * This function listens for user input in a specified ComboBox and returns it's typed text.
 * When the input field changes, it performs the following actions:
 * 1. Checks if the dropdown is already open by inspecting the `is-open` class on the combobox's
 *  container.
 * 2. If the dropdown is not open, it clicks the associated button to open the dropdown.
 * 3. Updates the Shiny input value (`app-inputs-primaryquery`) with the current input value.
 */
function ComboBoxquery(module, cbIdentity) {
  // Add an event listener for input events on the target combobox's input field
  $(document).on(
    'input',
    `[data-test="${cbIdentity}"] div input`,
    () => {
      const inputValue = document.querySelector(
        `[data-test="${cbIdentity}"] div input`,
      ).value;

      // Select the combobox container and button elements
      const dropdownContainer = document.querySelector(
        `[data-test="${cbIdentity}"] div`,
      );
      const dropdownButton = document.querySelector(
        `[data-test="${cbIdentity}"] div button`,
      );

      // Check if the dropdown is open by inspecting the 'is-open' class
      if (!dropdownContainer.classList.contains('is-open')) {
        dropdownButton.click();
      }

      // Update the corresponding Shiny input value with the current input
      Shiny.setInputValue(`app-${module}-${cbIdentity}-searchable_cb_query`, inputValue);
    },
  );
}

$(() => {
  // Waiting for `{shiny}` to be connected
  $(document).on('shiny:connected', () => {
    // Use delegated event binding
    ComboBoxquery('inputs', 'kpi_primary_region');
    ComboBoxquery('inputs', 'kpi_secondary_region');
    ComboBoxquery('compare', 'select_region');
    ComboBoxquery('compare', 'first_region-region_info');
    ComboBoxquery('compare', 'second_region-region_info');
  });
});
