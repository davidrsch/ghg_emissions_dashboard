const path = require('path')
const shinyCovPlugin = require('shiny.cov-cypress/plugin')

module.exports = {
  e2e: {
    setupNodeEvents(on, config) {
      shinyCovPlugin(on, config)
      return config
    },
    baseUrl: 'http://127.0.0.1:3333',
    supportFile: 'cypress/support/e2e.js',
    downloadsFolder: 'cypress/downloads',
    env: {
      // Must match the app_dir passed to shiny.cov::setup()/collect().
      shinyCovAppDir: path.resolve(__dirname, '..')
    }
  }
}
