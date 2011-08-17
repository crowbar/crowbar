Feature: Discover & Track Machines 
  In order to discover the system
  The machines,
  wants to check in w/ the crowbar server
  
  Scenario: Machine registers with : MAC address
    When a node registers with "01:23:45:67:89:ab:CD"
    Then I should see "h01-23-45-67-89-ab-cd."
  
  Scenario: Machine registers with - MAC address
    When a node registers with "01-23-45-67-89-ab-ef"
    Then I should see "h01-23-45-67-89-ab-ef."
  
  Scenario: Machine registers with Name (not MAC)
    When a node registers with "CUKETEST-basic"
      Then I should see "cuketest-basic."
      And I should not see "CUKETEST-basic."
      And I should not see "hCUKETEST-basic."
    
  Scenario: Machine state changes
    When I update node "CUKETEST-state" to "discovered"
    Then the "cuketest-state" state should be "discovered"
