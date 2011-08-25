Feature: Dashboard
  In order monitor the sytem
  The system operator, Oscar
  wants to be able to check the status of nodes

  Scenario: Check Status
    When I go to the "nodes" page
    Then I should see "a4-ba-db-70-f8-74"
    Then I should see "da4-ba-db-17-47-69"
    Then I should see "da4-ba-db-17-44-3f"
    Then I should see "admin"
    
  Scenario: Check Link 44-37
    Given I went to the "nodes" page
    When I click on the "da4-ba-db-17-44-3f" link
    Then I should see "Full Name"
    Then I should see "da4-ba-db-17-44-3f.dell.com"
    Then I should see "Ready"
    Then I should see "Allocated"
    Then I should see "a4-ba-db-70-f8-74 / 2"
    Then I should see "PowerEdge R710"
    Then I should see "Intel(R) Xeon(R) CPU E5530 @ 2.40GHz"
    Then I should see "47.26 GB"
    Then I should see "8"
    Then I should see "J2HK5M1"    
    
    
  Scenario: Check Link e0-c6
    Given I went to the "nodes" page
    When I click on the "d00-26-9e-cd-e0-c6" link
    Then I should see "Full Name"
    Then I should see "d00-26-9e-cd-e0-c6.dell.com"
    Then I should see "Ready"
    Then I should see "Allocated"
    Then I should see "a4-ba-db-88-92-07 / 4"
    Then I should see "PowerEdge C2100"
    Then I should see "Intel(R) Xeon(R) CPU E5540 @ 2.53GHz"
    Then I should see "23.59 GB"
    Then I should see "4"
    Then I should see "1234567"    