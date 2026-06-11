describe("Pivots tabs", () => {
  beforeEach(() => {
    cy.visit("/");
    //Importing a data to test
    cy.importing_data_flow("csv");
  });
  it("'Are pivots tabs visible'", () => {
    cy.get('[id="Pivot0-Tab0"]')
      .should('be.visible');
    cy.get('[id="Pivot0-Tab1"]')
      .should('be.visible');
    cy.get('[id="Pivot0-Tab2"]')
      .should('be.visible');
    cy.get('[id="Pivot0-Tab3"]')
      .should('be.visible');
  });
})