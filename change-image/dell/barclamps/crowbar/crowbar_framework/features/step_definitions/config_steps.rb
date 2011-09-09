Given /^the following configs:$/ do |configs|
  Config.create!(configs.hashes)
end

When /^I delete the (\d+)(?:st|nd|rd|th) config$/ do |pos|
  visit configs_path
  within("table tr:nth-child(#{pos.to_i+1})") do
    click_link "Destroy"
  end
end

Then /^I should see the following configs:$/ do |expected_configs_table|
  expected_configs_table.diff!(tableish('table tr', 'td,th'))
end
