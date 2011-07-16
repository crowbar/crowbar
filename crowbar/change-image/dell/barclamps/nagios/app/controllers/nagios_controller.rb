
class NagiosController < BarclampController
  def initialize
    @service_object = NagiosService.new logger
  end
end

