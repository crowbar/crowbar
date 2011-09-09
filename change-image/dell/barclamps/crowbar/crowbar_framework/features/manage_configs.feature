Feature: Manage configs
  In order to change way the system installs nodes 
  The system operator, Oscar
  wants to be able to review and set basic operating parameters
  
  Scenario: view base 
    When I am on the config page
    Then I should see "Configuration"
      And I should see "Crowbar"
      And I should see "DNS"
      And I should see "DHCP"
      And I should see "Private networks"
      And I should see "Public networks"
      And I should see "Admin networks"
      And I should see "Storage networks"
      And I should see "Installer"

  Scenario: ajax view
    When I am on the "dns" config ajax page
    Then I should see "Domain name"
      And I should see "pod.cloud.openstack.org"
      And I should not see "Crowbar"
      And I should not see "Home"
      And I should not see "Dell"  

  Scenario: change node name
    Given a node address "00:00:00:00:00:00:00:00"
    When I rename node "00-00-00-00-00-00-00-00" as "cuketest-rename"
      And I visit the node "00-00-00-00-00-00-00-00" page
    Then I should see "cuketest-rename"
      And I should not see "00-00-00-00-00-00-00-00"
  