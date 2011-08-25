Feature: Check Core Navigation
  In order use the system
  The system operator, Oscar
  wants to be able to navigate around
  
  Scenario: Home Page Nav
    When I go to the home page
    Then I should see "Dashboard"
    Then I should see "Barclamps"
    Then I should see "Active Roles"
    Then I should see "Proposals"
    Then I should see "Help"
    Then I should see "Chef Offline"
    Then I should see "CloudEdge Solution Team"
    Then I should not see "Error"
    
  Scenario: Dashboard Nav
    Given I am on the home page
    When I click on the "Dashboard" link
    Then I should see "nodes available in the system"
    Then I should see "admin"
  
  Scenario: Barclamps Nav
    Given I am on the home page
    When I click on the "Barclamps" link
    Then I should see "Barclamps"  
    Then I should see "crowbar"
    Then I should see "deployer"
    Then I should see "provisioner"
    Then I should see "dns"
    Then I should see "ntp"

  Scenario: Active Roles Nav
    Given I am on the home page
    When I click on the "Active Roles" link
    Then I should see "Active"  
    Then I should see "crowbar"
    Then I should see "deployer"
    Then I should see "provisioner"
    Then I should see "dns"
    Then I should see "ntp"
      
  Scenario: Proposals Nav
    Given I am on the home page
    When I click on the "Proposals" link
    Then I should see "Proposals"  
    Then I should see "crowbar"
    Then I should see "deployer"
    Then I should see "provisioner"
    Then I should see "dns"
    Then I should see "ntp"