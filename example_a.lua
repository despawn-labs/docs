local DisplayMode = {
  STATIC = "static",
  AUTO = "auto",
}

despawn.DisplayMode = DisplayMode

local SizingMode = {
  FILL = "fill",
  HUG = "hug",
  MANUAL = "manual",
}

despawn.SizingMode = SizingMode

local Direction = {
  HORIZONTAL = "horizontal",
  VERTICAL = "vertical",
}

despawn.Direction = Direction

local Alignment = {
  START = "start",
  MIDDLE = "middle",
  END = "end"
}

despawn.Alignment = Alignment

local Spacing = {
  AUTO = "auto",
}

despawn.Spacing = Spacing

local BackgroundClip = {
  BORDER_BOX = "border_box",
  PADDIN_BOX = "padding_box",
}

despawn.BackgroundClip = BackgroundClip

-- PanelAnimation Class
do
  local PanelAnimation
  PanelAnimation = {}
  PanelAnimation.__index = PanelAnimation

  AccessorFunc(PanelAnimation, "_start_time", "StartTime", FORCE_NUMBER)
  AccessorFunc(PanelAnimation, "_end_time", "EndTime", FORCE_NUMBER)
  AccessorFunc(PanelAnimation, "_data", "Data")

  function PanelAnimation:_Init(options)
    assert(options.end_time ~= nil or options.duration ~= nil, "options.end_time or options.duration must be set")

    self:SetStartTime(options.start_time or SysTime())
    self:SetEndTime(options.end_time or SysTime() + options.duration)
  end

  function PanelAnimation:Think(panel, fraction)
    if self._canceled then 
      return
    end

    if self.OnThink then
      self:OnThink(panel, fraction)
    end
  end

  function PanelAnimation:Cancel()
    self._canceled = true
  end

  function PanelAnimation:IsCanceled()
    return self._canceled
  end

  despawn.Constructor(PanelAnimation)
  despawn.PanelAnimation = PanelAnimation
end

-- Helper function which returns "h" when direction is "horizontal" and "v" otherwise.
local function H(direction, h, v)
  return direction == despawn.Direction.HORIZONTAL and h or v
end

-- Helper function which calls "h" when direction is "horizontal" and "v" otherwise, "t" is used as the caller table.
local function CH(direction, t, h, v, ...)
  if direction == despawn.Direction.HORIZONTAL then 
    return t[h](t, ...)
  else
    return t[v](t, ...)
  end
end

local function RegisterPanelVariant(name, base)
  local PANEL = vgui.Register(name, {}, base)

  AccessorFunc(PANEL, "_display_mode", "DisplayMode")

  AccessorFunc(PANEL, "_direction", "Direction")

  AccessorFunc(PANEL, "_width_sizing_mode", "WidthSizingMode")
  AccessorFunc(PANEL, "_height_sizing_mode", "HeightSizingMode")

  AccessorFunc(PANEL, "_primary_factor", "PrimaryFactor")

  AccessorFunc(PANEL, "_primary_alignment", "PrimaryAlignment")
  AccessorFunc(PANEL, "_cross_alignment", "CrossAlignment")

  --- { left, right, top, bottom }
  AccessorFunc(PANEL, "_padding", "Padding")
  AccessorFunc(PANEL, "_spacing", "Spacing")

  AccessorFunc(PANEL, "_background_shadow_enabled", "BackgroundShadowEnabled")

  AccessorFunc(PANEL, "_background_enabled", "BackgroundEnabled")
  AccessorFunc(PANEL, "_background_clip", "BackgroundClip")
  AccessorFunc(PANEL, "_background_color", "BackgroundColor")

  AccessorFunc(PANEL, "_border_enabled", "BorderEnabled")
  AccessorFunc(PANEL, "_border_color", "BorderColor")
  AccessorFunc(PANEL, "_border_width", "BorderWidth")

  --- { top_left, top_right, bottom_left, bottom_right }
  AccessorFunc(PANEL, "_border_radius", "BorderRadius")

  -- Hooks
  do
    function PANEL:Init()
      local cfg = despawn.config.ui.panel

      self:SetDisplayMode(despawn.DisplayMode.STATIC)

      self:SetDirection(despawn.Direction.HORIZONTAL)

      self:SetSizingMode(despawn.SizingMode.MANUAL, despawn.SizingMode.MANUAL)

      self:SetPrimaryFactor(1)

      self:SetAlignment(despawn.Alignment.START, despawn.Alignment.START)

      self:SetPadding({ 0, 0, 0, 0 })
      self:SetSpacing(0)

      self:SetBorderRadius(despawn.ScaleT({ 8, 8, 8, 8 }))

      self:SetBackgroundClip(despawn.BackgroundClip.BORDER_BOX)

      self:SetBackgroundEnabled(false)
      self:SetBackgroundColor(cfg.background)

      self:SetBorderEnabled(false)
      self:SetBorderColor(cfg.border)
      self:SetBorderWidth(1)

      self._animations = {}
    end

    function PANEL:PerformLayout(width, height)
      local parent = self:GetParent()

      if parent.PerformSizing then
        parent:PerformSizing()
      end
    
      self:_ResizeFillChildren()
      self:_LayoutChildren()
    end

    function PANEL:PerformSizing()
      self:_Hug()
    end

    function PANEL:Paint(w, h)
      self:_CalculateClippingRegion()

      local cp = self:GetBackgroundClip() == despawn.BackgroundClip.PADDING_BOX

      local ox, oy = self:GetBorderWidth(), self:GetBorderWidth()
      local ow, oh = w - (self:GetBorderWidth() * 2), h - (self:GetBorderWidth() * 2)

      local wrapper = function(f)
        f()
      end

      if self:GetBackgroundShadowEnabled() then
        wrapper = function(f)
          despawn.WithShadow({
            spread = 4,
            intensity = 4,
            blur = 4,
            alpha = 200,
          }, function()
            f()
          end)
        end
      end

      wrapper(function()
        local aug_radii = despawn.Map(ipairs, self:GetBorderRadius(), function(radi)
          return istable(radi) and {
            x = math.max(radi.x - self:GetBorderWidth(), 0),
            y = math.max(radi.y - self:GetBorderWidth(), 0)
          } or math.max(radi - self:GetBorderWidth(), 0)
        end)

        despawn.SetTargetPanel(self)
        render.SetScissorRect(self._clip.x1, self._clip.y1, self._clip.x2, self._clip.y2, true)
        
        if self:GetBackgroundEnabled() then
          despawn.DrawRect(
            cp and ox or 0, cp and oy or 0,
            cp and ow or w, cp and oh or h,
            self:GetBackgroundColor(),
            unpack(cp and aug_radii or self:GetBorderRadius())
          )
        end

        render.SetScissorRect(0, 0, 0, 0, false)
        despawn.SetTargetPanel(nil)

        if self:GetBorderEnabled() then
          despawn.Mask(function()
            despawn.DrawRect(
              0, 0,
              w, h,
              self:GetBorderColor(),
              unpack(self:GetBorderRadius())
            )
          end, function()
            despawn.DrawRect(
              self:GetBorderWidth(), self:GetBorderWidth(),
              ow, oh,
              color_white,
              unpack(aug_radii)
            )
          end, despawn.MaskType.STAMP, unpack(self:GetBackgroundShadowEnabled() and { self:LocalToScreen() } or { 0, 0 }))
        end
      end)
    end
    
    function PANEL:Think()
      self:_ProcessAnimations()

      if self._should_perform_sizing then
        self:PerformSizing()
        self._should_perform_sizing = false
      end
    end

    function PANEL:OnChildAdded()
      if self:GetWidthSizingMode() == despawn.SizingMode.HUG or self:GetHeightSizingMode() == despawn.SizingMode.HUG then
        self:PerformSizing()
      end
    end
  end

  -- Public
  do
    function PANEL:GetVisibleChildren()
      return despawn.Filter(ipairs, self:GetChildren(), function(value) return value:IsVisible() end)
    end

    function PANEL:InvalidateSizing(immediate)
      if immediate then
        self:PerformSizing()
        self._should_perform_sizing = false
        return
      end

      self._should_perform_sizing = true
    end

    despawn.Hook(PANEL, "SetDisplayMode", function(self, display_mode)
      -- Trigger a re-layout for us and the parent.
      if display_mode == despawn.DisplayMode.AUTO then
        self:GetParent():InvalidateLayout()
        self:InvalidateSizing()
      end
    end)

    despawn.Hook(PANEL, "SetDirection", function(self, direction)
      -- Trigger a re-layout.
      self:InvalidateLayout()
    end)

    despawn.Hook(PANEL, "SetPrimaryFactor", function(self, direction)
      self:GetParent():InvalidateLayout()
    end)

    function PANEL:SetSizingMode(width, height)
      assert(isstring(width), "width must be a string (despawn.SizingMode)")
      assert(isstring(height), "height must be a string (despawn.SizingMode)")

      self._width_sizing_mode = width
      self._height_sizing_mode = height

      -- Trigger the parent to re-layout.
      if width == despawn.SizingMode.FILL or height == despawn.SizingMode.FILL then
        self:GetParent():InvalidateLayout()
      end

      -- Trigger us to hug elements.
      if width == despawn.SizingMode.HUG or height == despawn.SizingMode.HUG then
        self:InvalidateSizing()
      end
    end

    function PANEL:SetWidthSizingMode(width)
      self:SetSizingMode(width, self:GetHeightSizingMode())
    end

    function PANEL:SetHeightSizingMode(height)
      self:SetSizingMode(self:GetWidthSizingMode(), height)
    end

    function PANEL:SetAlignment(primary, cross)
      assert(isstring(primary), "primary must be a string (despawn.Alignment)")
      assert(isstring(cross), "cross must be a string (despawn.Alignment)")

      self._primary_alignment = primary
      self._cross_alignment = cross

      self:InvalidateLayout()
    end

    function PANEL:SetPrimaryAlignment(alignment)
      self:SetAlignment(alignment, self:GetCrossAlignment())
    end

    function PANEL:SetCrossAlignment(alignment)
      self:SetAlignment(self:GetPrimaryAlignment(), alignment)
    end

    despawn.Hook(PANEL, "SetSpacing", function(self, spacing)
      -- Trigger a re-size if we're hugging on the primary axis.
      if CH(self:GetDirection(), self, "GetWidthSizingMode", "GetHeightSizingMode") == despawn.SizingMode.HUG then
        self:InvalidateSizing()
      end
    end)

    despawn.Hook(PANEL, "SetPadding", function(self, padding)
      -- Trigger a re-size if we're hugging on either axis.
      if self:GetWidthSizingMode() == despawn.SizingMode.HUG or self:GetHeightSizingMode() == despawn.SizingMode.HUG then
        self:InvalidateSizing()
      end
    end)

    function PANEL:GetClippingRegion()
      return self._clip
    end

    function PANEL:GetContentWidth(spacing)
      local padding = self:GetPadding()
      local spacing = spacing ~= nil and spacing or self:GetSpacing()
      local children = self:GetVisibleChildren()

      if isstring(spacing) then
        spacing = 0
      end

      local total_width = despawn.Reduce(ipairs, children, function(a, child)
        if child:GetWide() > 0 then
          return a + child:GetWide() + spacing
        else
          return a
        end
      end, 0)

      if total_width > 0 then
        total_width = total_width - spacing
      end

      total_width = total_width + padding[1] + padding[2]

      return total_width
    end

    function PANEL:GetContentHeight(spacing)
      local padding = self:GetPadding()
      local spacing = spacing ~= nil and spacing or self:GetSpacing()
      local children = self:GetVisibleChildren()

      if isstring(spacing) then
        spacing = 0
      end

      local total_height = despawn.Reduce(ipairs, children, function(a, child)
        if child:GetTall() > 0 then
          return a + child:GetTall() + spacing
        else
          return a
        end
      end, 0)

      if total_height > 0 then
        total_height = total_height - spacing
      end

      total_height = total_height + padding[3] + padding[4]

      return total_height
    end

    function PANEL:Animate(key, animation)
      self._animations[key] = animation
    end

    function PANEL:AnimateVar(var, from, to, options)
      assert(var and isstring(var), "var must be a string")
      assert(self[var], "var must index a variable that exists on self")

      local cfg = despawn.config.ui.animations

      options = options or {}
      options.duration = options.duration or cfg.duration
      options.ease = options.ease or cfg.ease
      options.lerp = options.lerp or Lerp

      local animation = despawn.PanelAnimation({
        duration = options.duration,
      })

      animation:SetData(table.Merge({
        var = var,
        from = from,
        to = to,
      }, options))

      animation.OnThink = function(this, panel, fraction)
        local data = this:GetData()
        local new_fraction = data.ease(fraction)

        panel[data.var] = data.lerp(new_fraction, data.from, data.to)
      end

      animation.OnEnd = function(this, panel)
        local data = this:GetData()

        panel[data.var] = data.to

        if data.on_end then
          data.on_end(panel)
        end
      end

      self:Animate("var:" .. var, animation)

      return animation
    end

    function PANEL:AnimateMethod(method, from, to, options)
      assert(method and isstring(method), "method must be a string")
      assert(self[method], "method must index a methodiable that exists on self")

      local cfg = despawn.config.ui.animations

      options = options or {}
      options.duration = options.duration or cfg.duration
      options.ease = options.ease or cfg.ease
      options.lerp = options.lerp or Lerp

      local animation = despawn.PanelAnimation({
        duration = options.duration,
      })

      animation:SetData(table.Merge({
        method = method,
        from = from,
        to = to,
      }, options))

      animation.OnThink = function(this, panel, fraction)
        local data = this:GetData()
        local new_fraction = data.ease(fraction)

        panel[data.method](panel, data.lerp(new_fraction, data.from, data.to))
      end

      animation.OnEnd = function(this, panel)
        local data = this:GetData()

        panel[data.method](panel, data.to)

        if data.on_end then
          data.on_end(panel)
        end
      end

      self:Animate("method:" .. method, animation)

      return animation
    end

    function PANEL:AnimateColorVar(var, from, to, options)
      return self:AnimateVar(var, from, to, table.Merge(options or {}, {
        lerp = despawn.LerpColor
      }))
    end

    function PANEL:AnimateBackgroundColor(from, to, options)
      return self:AnimateMethod("SetBackgroundColor", from, to, table.Merge(options or {}, {
        lerp = despawn.LerpColor,
      }))
    end

    function PANEL:AnimateBorderColor(from, to, options)
      return self:AnimateMethod("SetBorderColor", from, to, table.Merge(options or {}, {
        lerp = despawn.LerpColor,
      }))
    end

    function PANEL:AnimateAlpha(from, to, options)
      return self:AnimateMethod("SetAlpha", from, to, options)
    end

    function PANEL:AnimateHeight(from, to, options)
      return self:AnimateMethod("SetHeight", from, to, options)
    end

    function PANEL:AnimateWidth(from, to, options)
      return self:AnimateMethod("SetWidth", from, to, options)
    end
  end

  -- Private
  do
    function PANEL:_ResizeFillChildren()
      if self:GetDisplayMode() ~= despawn.DisplayMode.AUTO then
        return
      end

      local direction = self:GetDirection()
      local padding = self:GetPadding()
      local spacing = self:GetSpacing()
      local children = self:GetVisibleChildren()

      if isstring(spacing) then
        spacing = 0
      end

      local remaining_primary_length = self[H(direction, "GetWide", "GetTall")](self)
      local cross_length = self[H(direction, "GetTall", "GetWide")](self)
      
      local n_fill_children = 0
      local total_factor = 0
      
      for _, child in ipairs(children) do
        local get_primary_sizing_mode = child[H(direction, "GetWidthSizingMode", "GetHeightSizingMode")]

        if not get_primary_sizing_mode or get_primary_sizing_mode(child) ~= despawn.SizingMode.FILL then
          local amount = child[H(direction, "GetWide", "GetTall")](child)
          remaining_primary_length = remaining_primary_length - amount
        end

        if get_primary_sizing_mode and get_primary_sizing_mode(child) == despawn.SizingMode.FILL then
          n_fill_children = n_fill_children + 1
          total_factor = total_factor + child:GetPrimaryFactor()
        end
      end

      local total_spacing = spacing * (#children - 1)

      remaining_primary_length = remaining_primary_length - padding[H(direction, 1, 3)] - padding[H(direction, 2, 4)] - total_spacing

      for _, child in ipairs(children) do
        local get_primary_sizing_mode = child[H(direction, "GetWidthSizingMode", "GetHeightSizingMode")]
        local get_cross_sizing_mode = child[H(direction, "GetHeightSizingMode", "GetWidthSizingMode")]
        
        if get_primary_sizing_mode(child) == despawn.SizingMode.FILL then
          child[H(direction, "SetWide", "SetTall")](child, math.Round((remaining_primary_length) * (child:GetPrimaryFactor() / total_factor)))
        end

        if get_cross_sizing_mode(child) == despawn.SizingMode.FILL then
          child[H(direction, "SetTall", "SetWide")](child, cross_length - padding[H(direction, 3, 1)] - padding[H(direction, 4, 2)])
        end
      end
    end

    function PANEL:_Hug()
      if self:GetDisplayMode() ~= despawn.DisplayMode.AUTO then
        return
      end

      local direction = self:GetDirection()
      local padding = self:GetPadding()
      local spacing = self:GetSpacing()
      local children = self:GetVisibleChildren()

      if CH(direction, self, "GetWidthSizingMode", "GetHeightSizingMode") == despawn.SizingMode.HUG then
        local total_primary_length = despawn.Reduce(ipairs, children, function(a, child)
          local length = CH(direction, child, "GetWide", "GetTall")
          if length > 0 then
            return a + length + spacing
          else
            return a
          end
        end, 0)

        if total_primary_length > 0 then
          total_primary_length = total_primary_length - spacing
        end

        total_primary_length = total_primary_length + H(direction, padding[1], padding[3]) + H(direction, padding[2], padding[4])

        CH(direction, self, "SetWide", "SetTall", total_primary_length)
      end

      if CH(direction, self, "GetHeightSizingMode", "GetWidthSizingMode") == despawn.SizingMode.HUG then
        local cross_length = 
          H(direction, padding[3], padding[1]) + H(direction, padding[4], padding[2])

        local largest_length = 0
        for _, child in ipairs(children) do
          local length = CH(direction, child, "GetTall", "GetWide")

          if length > largest_length then
            largest_length = length
          end
        end

        cross_length = cross_length + largest_length

        CH(direction, self, "SetTall", "SetWide", cross_length)
      end
    end

    function PANEL:_LayoutChildren()
      if self:GetDisplayMode() ~= despawn.DisplayMode.AUTO then
        return
      end

      local direction = self:GetDirection()
      local spacing = self:GetSpacing()

      local primary_length = CH(direction, self, "GetWide", "GetTall")
      local cross_length = CH(direction, self, "GetTall", "GetWide")

      if spacing == despawn.Spacing.AUTO then
        spacing = (primary_length - CH(direction, self, "GetContentWidth", "GetContentHeight", 0)) / (#self:GetVisibleChildren() - 1)
      end

      local padding = self:GetPadding()
      local primary_alignment = self:GetPrimaryAlignment()
      local cross_alignment = self:GetCrossAlignment()

      local primary_cursor = H(direction, padding[1], padding[3])
      local cross_cursor = H(direction, padding[3], padding[1])

      local content_primary_length = CH(direction, self, "GetContentWidth", "GetContentHeight")

      if self:GetSpacing() == despawn.Spacing.AUTO then
        content_primary_length = primary_length
      end

      if primary_alignment == despawn.Alignment.MIDDLE then
        primary_cursor = primary_cursor + (primary_length / 2) - (content_primary_length / 2)
      elseif primary_alignment == despawn.Alignment.END then
        primary_cursor = primary_length - content_primary_length + H(direction, padding[4], padding[2])
      end

      for _, child in ipairs(self:GetVisibleChildren()) do
        if cross_alignment == despawn.Alignment.MIDDLE then
          cross_cursor = cross_length / 2 - CH(direction, child, "GetTall", "GetWide") / 2
        elseif cross_alignment == despawn.Alignment.END then
          cross_cursor = cross_length - CH(direction, child, "GetTall", "GetWide") - H(direction, padding[2], padding[1])
        end

        child:SetPos(
          H(direction, primary_cursor, cross_cursor),
          H(direction, cross_cursor, primary_cursor)
        )

        primary_cursor = primary_cursor + CH(direction, child, "GetWide", "GetTall") + spacing
      end
    end

    function PANEL:_CalculateClippingRegion()
      local sx, sy = self:LocalToScreen()

      local parent = self:GetParent()
      local parent_clip = parent._clip
      if parent_clip then
        local lx, ly = sx - parent_clip.x1, sy - parent_clip.y1
        self._clip = {
          x1 = lx < 0 and parent_clip.x1 or sx,
          y1 = ly < 0 and parent_clip.y1 or sy,
          x2 = math.min(sx + self:GetWide(), parent_clip.x2),
          y2 = math.min(sy + self:GetTall(), parent_clip.y2)
        }
      else
        self._clip = { x1 = sx, y1 = sy, x2 = sx + self:GetWide(), y2 = sy + self:GetTall() }
      end
    end

    function PANEL:_ProcessAnimations()
      local ts = SysTime()

      for key, animation in pairs(self._animations) do
        local start_time = animation:GetStartTime()
        local end_time = animation:GetEndTime()

        if ts >= end_time then
          if animation.OnEnd then
            if animation:IsCanceled() then
              continue
            end

            animation:OnEnd(self)
          end

          self._animations[key] = nil

          continue 
        end

        local fraction = (ts - start_time) / (end_time - start_time)

        animation:Think(self, fraction)
      end
    end
  end
end

---
--- @panel despawn.Panel
---
--- @accessor (_display_mode: despawn.DisplayMode) [DisplayMode] The display-mode or method in which children will be managed.
---
--- @accessor (_direction: despawn.Direction) [Direction] The direction in which to layout the children, when using the auto display-mode.
---
--- @accessor (_width_sizing_mode: despawn.SizingMode) [WidthSizingMode] The sizing mode to use on the width of the panel in auto display-mode.
--- @accessor (_height_sizing_mode: despawn.SizingMode) [HeightSizingMode] The sizing mode to use on the height of the panel in auto display-mode.
---
--- @accessor (_primary_alignment: despawn.Alignment) [PrimaryAlignment] The primary alignment of the label, when using auto display-mode..
--- @accessor (_cross_alignment: despawn.Alignment) [CrossAlignment] The cross alignment of the label, when using auto display-mode..
---
--- @accessor (_padding: table) [Padding] A table containing the four padding values (left, right, top, bottom), when using auto display-mode..
--- @accessor (_spacing: number) [Spacing] The spacing between elements, when using auto display-mode.
---
--- @accessor (_background_enabled: boolean) [BackgroundEnabled] Whether or not the background should be drawn.
--- @accessor (_background_color: Color) [BackgroundColor] The color of the background.
--- @accessor (_background_radius: number) [BackgroundRadius] The corner radius of the background.
---
--- The root container type used by the component library.
--- Capable of managing children's position and size dynamically.
---
RegisterPanelVariant("despawn.Panel", "Panel")

---
--- @panel despawn.EditablePanel
---
--- @accessor (_display_mode: despawn.DisplayMode) [DisplayMode] The display-mode or method in which children will be managed.
---
--- @accessor (_direction: despawn.Direction) [Direction] The direction in which to layout the children, when using the auto display-mode.
---
--- @accessor (_width_sizing_mode: despawn.SizingMode) [WidthSizingMode] The sizing mode to use on the width of the panel in auto display-mode.
--- @accessor (_height_sizing_mode: despawn.SizingMode) [HeightSizingMode] The sizing mode to use on the height of the panel in auto display-mode.
---
--- @accessor (_primary_alignment: despawn.Alignment) [PrimaryAlignment] The primary alignment of the label, when using auto display-mode..
--- @accessor (_cross_alignment: despawn.Alignment) [CrossAlignment] The cross alignment of the label, when using auto display-mode..
---
--- @accessor (_padding: table) [Padding] A table containing the four padding values (left, right, top, bottom), when using auto display-mode..
--- @accessor (_spacing: number) [Spacing] The spacing between elements, when using auto display-mode.
---
--- @accessor (_background_enabled: boolean) [BackgroundEnabled] Whether or not the background should be drawn.
--- @accessor (_background_color: Color) [BackgroundColor] The color of the background.
--- @accessor (_background_radius: number) [BackgroundRadius] The corner radius of the background.
---
--- The root container type used by the component library.
--- Capable of managing children's position and size dynamically.
---
RegisterPanelVariant("despawn.EditablePanel", "EditablePanel")

despawn.DebugPanel("despawn.Frame", function(root)
  for i = 1, 5 do
    local pnl = vgui.Create("despawn.Panel", root:GetBody())
    pnl:SetBackgroundColor(Color(255, 0, 0))
    pnl:SetBackgroundEnabled(true)

    if i == 1 then
      pnl:SetVisible(false)
    end
  end

  root:SetSize(500, 500)
  root:Center()
  root:Open()
end)