Feature: Check Core Navigation
  In order use the system
  The system operator, Oscar
  wants to be able to navigate around
  
  Scenario: Home Page Nav
    When I am on the home page
    Then I should see "Dashboard"
      And I should see "Barclamps"
      And I should see "Active Roles"
      And I should see "Proposals"
      And I should see "Chef Offline"
      And I should not see "Error"
    
  Scenario: Dashboard Nav
    When I am on the home page
      and I click on the "Dashboard" link
    Then I should see "nodes available in the system"
      And I should see "admin"
  
  Scenario: Barclamps Nav
    When I am on the home page
      and I click on the "Barclamps" link
    Then I should see "Barclamps"  
      And I should see "crowbar"
      And I should see "deployer"
      And I should see "provisioner"
      And I should see "dns"
      And I should see "ntp"

  Scenario: Active Roles Nav
    When I am on the home page
      and I click on the "Active Roles" link
    Then I should see "Active"  
      And I should see "crowbar"
      And I should see "deployer"
      And I should see "provisioner"
      And I should see "dns"
      And I should see "ntp"
      
  Scenario: Proposals Nav
    When I am on the home page
      and I click on the "Proposals" link
    Then I should see "Proposals"  
      And I should see "crowbar"
      And I should see "deployer"
      And I should see "provisioner"
      And I should see "dns"
      And I should see "ntp"
