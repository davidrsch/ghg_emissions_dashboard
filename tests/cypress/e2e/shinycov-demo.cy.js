describe('shiny.cov demo', () => {
  it('renders tabs, discovers UI, and logs a couple of interactions', () => {
    cy.visit('/')
    cy.get('[id^="Pivot0-Tab"]').should('have.length.at.least', 4)

    // Overview
    cy.get('[id="Pivot0-Tab1"]').click()
    cy.wait(4000)

    // Log an input interaction and an output verification (shiny.cov's
    // Cypress adapter logs explicitly via cy.shinyCovInteract).
    cy.shinyCovInteract('#app-inputs-kpi_years-input', 'set_inputs', '2020')
    cy.shinyCovInteract('#app-keymetrics-tghge', 'get_text')

    // Compare
    cy.get('[id="Pivot0-Tab2"]').click()
    cy.wait(3000)

    // Explore the Planet
    cy.get('[id="Pivot0-Tab3"]').click()
    cy.wait(3000)
  })
})
