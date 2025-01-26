/**
 * Function: ComboBoxquery
 * 
 * This function listens for user input in a specified ComboBox and returns it's typed text.
 * When the input field changes, it performs the following actions:
 * 1. Checks if the dropdown is already open by inspecting the `is-open` class on the combobox's container.
 * 2. If the dropdown is not open, it clicks the associated button to open the dropdown.
 * 3. Updates the Shiny input value (`app-inputs-primaryquery`) with the current input value.
 */
function ComboBoxquery(cbidentified) {
  // Add an event listener for input events on the target combobox's input field
  $(document).on('input', '[data-test="' + cbidentified + '"] div input', function () {
    const inputValue = this.value; // The current value of the input field
    console.log('Input changed:', inputValue); // Log the value for debugging

    // Select the combobox container and button elements
    const dropdownContainer = document.querySelector('[data-test="' + cbidentified + '"] div');
    const dropdownButton = document.querySelector('[data-test="' + cbidentified + '"] div button');

    // Check if the dropdown is open by inspecting the 'is-open' class
    if (!dropdownContainer.classList.contains('is-open')) {
      console.log('Dropdown is not open. Clicking the button to open it...');
      dropdownButton.click(); // Open the dropdown by simulating a button click
    } else {
      console.log('Dropdown is already open. No action needed.');
    }
    
    // Update the corresponding Shiny input value with the current input
    Shiny.setInputValue('app-inputs-' + cbidentified + '_query', inputValue);
  });
}


$(function(){ 
  // Waiting for `{shiny}` to be connected
  $(document).on('shiny:connected', function(event) {
    // Use delegated event binding
    ComboBoxquery('kpi_primary_region');
    ComboBoxquery('kpi_secondary_region');
  });  
});
