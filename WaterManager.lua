dofile( "$SURVIVAL_DATA/Scripts/game/survival_constants.lua" )

WaterManager = class( nil )

function WaterManager.onCreate( self )
	self.cells = {}

	self.triggeredCharacters = {}
	self.triggeredBodies = {}
end

function WaterManager.sv_onCreate( self )
	if sm.isHost then
		self:onCreate()
	end
end

function WaterManager.cl_onCreate( self )
	if not sm.isHost then
		self:onCreate()
	end
end


-- ################################ World cell callbacks ################################

-- Loaded/Reloaded

function WaterManager.onCellLoaded( self, x, y )
	--print("--- loading water objs on cell " .. x .. ":" .. y .. " ---")
	local cellKey = CellKey(x, y)
	local waterNodes = sm.cell.getNodesByTag( x, y, "WATER" )
	local chemicalNodes = sm.cell.getNodesByTag( x, y, "CHEMICALS" )

	if #waterNodes > 0 or #chemicalNodes > 0 then
		self.cells[cellKey] = {}
	end

	local idx = 1
	if #waterNodes > 0 then
		for _, node in ipairs( waterNodes ) do
			local areaTrigger = sm.areaTrigger.createBoxWater( node.scale * 0.5, node.position, node.rotation, sm.areaTrigger.filter.character + sm.areaTrigger.filter.dynamicBody + sm.areaTrigger.filter.areaTrigger, { water = true } )
			areaTrigger:bindOnEnter( "trigger_onEnterWater", self )
			areaTrigger:bindOnExit( "trigger_onExitWater", self )
			areaTrigger:bindOnStay( "trigger_onStayWater", self )
			areaTrigger:bindOnProjectile( "trigger_onProjectile", self )
			self.cells[cellKey][idx] = areaTrigger
			idx = idx + 1
		end
	end

	if #chemicalNodes > 0 then
		for _, node in ipairs( chemicalNodes ) do
			local areaTrigger = sm.areaTrigger.createBoxWater( node.scale * 0.5, node.position, node.rotation, sm.areaTrigger.filter.character + sm.areaTrigger.filter.dynamicBody + sm.areaTrigger.filter.areaTrigger, { chemical = true } )
			areaTrigger:bindOnEnter( "trigger_onEnterWater", self )
			areaTrigger:bindOnExit( "trigger_onExitWater", self )
			areaTrigger:bindOnStay( "trigger_onStayWater", self )
			areaTrigger:bindOnProjectile( "trigger_onProjectile", self )
			self.cells[cellKey][idx] = areaTrigger
			idx = idx + 1
		end
	end
end

function WaterManager.sv_onCellLoaded( self, x, y )
	if sm.isHost then
		self:onCellLoaded( x, y )
	end
end

function WaterManager.sv_onCellReloaded( self, x, y )
	if sm.isHost then
		self:onCellLoaded( x, y )
	end
end

function WaterManager.cl_onCellLoaded( self, x, y )
	if not sm.isHost then
		self:onCellLoaded( x, y )
	end
end


-- Unloaded

function WaterManager.onCellUnloaded( self, x, y )
	--print("--- unloading water objs on cell " .. x .. ":" .. y .. " ---")
	local cellKey = CellKey(x, y)

	if self.cells[cellKey] then
		for _, trigger in ipairs( self.cells[cellKey] ) do
			sm.areaTrigger.destroy( trigger )
		end
		self.cells[cellKey] = nil
	end
end

function WaterManager.sv_onCellUnloaded( self, x, y )
	if sm.isHost then
		self:onCellUnloaded( x, y )
	end
end

function WaterManager.cl_onCellUnloaded( self, x, y )
	if not sm.isHost then
		self:onCellUnloaded( x, y )
	end
end


-- ################################ Update callbacks ################################

function WaterManager.onFixedUpdate( self )
	-- Reset tracking of triggered objects during previous tick
	self.triggeredCharacters = {}
	self.triggeredBodies = {}
end

function WaterManager.sv_onFixedUpdate( self )
	if sm.isHost then
		self:onFixedUpdate()
	end
end

function WaterManager.cl_onFixedUpdate( self )
	if not sm.isHost then
		self:onFixedUpdate()
	end
end

-- ################################ AreaTrigger callbacks ################################

local function PlaySplashEffect( pos, velocity, mass )

	local energy = 0.5*velocity:length()*velocity:length()*mass

	--print( "Velocity: ", velocity:length() )
	--print( "Mass: ", mass )
	--print( "Energy: ", energy)

	local params = {
		["Size"] = min( 1.0, mass / 76800.0 ),
		["Velocity_max_50"] = velocity:length(),
		["Phys_energy"] = energy / 1000.0
	}

	if energy > 8000 then
		sm.effect.playEffect( "Water - HitWaterMassive", pos, sm.vec3.zero(), sm.quat.identity(), sm.vec3.one(), params )
	elseif energy > 4000 then
		sm.effect.playEffect( "Water - HitWaterBig", pos, sm.vec3.zero(), sm.quat.identity(), sm.vec3.one(), params )
	elseif energy > 150 then
		sm.effect.playEffect( "Water - HitWaterSmall", pos, sm.vec3.zero(), sm.quat.identity(), sm.vec3.one(), params )
	elseif energy > 1 then
		sm.effect.playEffect( "Water - HitWaterTiny", pos, sm.vec3.zero(), sm.quat.identity(), sm.vec3.one(), params )
	end
		
end

local function CalulateForce( waterHeightPos, worldPosition, worldRotation, velocity, halfExtent, mass )
	-- Setup all the corners of the shape's bounding box
	local volume = ( halfExtent.x * 2.0 * halfExtent.y * 2.0 * halfExtent.z * 2.0 )

	local corners = {}
	corners[1] = worldPosition + worldRotation * sm.vec3.new( halfExtent.x, halfExtent.y, halfExtent.z )
	corners[2] = worldPosition + worldRotation * sm.vec3.new( halfExtent.x, halfExtent.y, -halfExtent.z )
	corners[3] = worldPosition + worldRotation * sm.vec3.new( halfExtent.x, -halfExtent.y, halfExtent.z )
	corners[4] = worldPosition + worldRotation * sm.vec3.new( halfExtent.x, -halfExtent.y, -halfExtent.z )
	corners[5] = worldPosition + worldRotation * sm.vec3.new( -halfExtent.x, halfExtent.y, halfExtent.z )
	corners[6] = worldPosition + worldRotation * sm.vec3.new( -halfExtent.x, halfExtent.y, -halfExtent.z )
	corners[7] = worldPosition + worldRotation * sm.vec3.new( -halfExtent.x, -halfExtent.y, halfExtent.z )
	corners[8] = worldPosition + worldRotation * sm.vec3.new( -halfExtent.x, -halfExtent.y, -halfExtent.z )
	-- Sort so the lowest corners go first
	table.sort( corners, function ( c1, c2 ) return c1.z < c2.z end )

	-- Check which of the lowest corners are under water and calculate their total depth
	local submergedPoints = {}
	local totalDepth = 0
	local avgDepth = 0
	for _, corner in pairs( { corners[1], corners[2], corners[3], corners[4] } ) do
		if corner.z < waterHeightPos then
			totalDepth = totalDepth + ( waterHeightPos - corner.z )
			submergedPoints[#submergedPoints+1] = corner
		end
	end
	avgDepth = totalDepth * 0.25

	-- Approximate how much of the shape is submerged to calculate the displaced volume
	local bottomArea = 0.5 * ( ( corners[1] - corners[2] ):cross( corners[1] - corners[3] ):length() + ( corners[4] - corners[2] ):cross( corners[4] - corners[3] ):length() )
	local displacedVolume = math.min( ( bottomArea * avgDepth ), volume )

	-- Buoyancy force formula
	local gravity = 10
	local fluidDensity = 500 -- density level 4
	local buoyancyForce = fluidDensity * displacedVolume * gravity

	-- Apply buoyancy force to the shape's body using the shape's CoM as an offset
	local force = sm.vec3.new( 0, 0, buoyancyForce )
	local result = force * 0.025;

	-- Diminishing force in water, acts as water resistance
	if velocity:length() >= FLT_EPSILON then
		local direction = velocity:normalize()
		local magnitude = velocity:length()

		local antiVelocity = -direction * ( magnitude * 0.15 )
		if velocity.z > 0 then
			antiVelocity = -direction * ( magnitude * 0.15 ) --Ascending
		end
		local antiForceVector = ( antiVelocity / 0.025 ) * mass

		local horizontalDiminishScale = 0.1
		antiForceVector.x = antiForceVector.x * horizontalDiminishScale
		antiForceVector.y = antiForceVector.y * horizontalDiminishScale
		result = result + ( antiForceVector * 0.025 )
	end

	return result
end

local function UpdateCharacterInWater( trigger, character )
	-- Update swim state
	local waterHeightPos = trigger:getWorldMax().z
	--local characterFloatHeight = character.worldPosition.z + character:getHeight() * 0.15
	--local characterDiveHeight = character.worldPosition.z + character:getHeight() * 0.5
	local characterFloatOffset = 0.2 + ( character:isCrouching() and 0.4 or 0.0 )
	local characterFloatHeight = character.worldPosition.z + characterFloatOffset
	local characterDiveOffset = 0.7 + ( character:isCrouching() and 0.4 or 0.0 )
	local characterDiveHeight = character.worldPosition.z + characterDiveOffset
	if sm.isHost and character:getCanSwim() then
		-- Update swimming state
		if not character:isSwimming() then
			if waterHeightPos > characterFloatHeight then
				character:setSwimming( true )
			end
		else
			if waterHeightPos <= characterFloatHeight then
				character:setSwimming( false )
			end
		end
		-- Update diving state
		if not character:isDiving() then
			if waterHeightPos > characterDiveHeight then
				character:setDiving( true )
			end
		else
			if waterHeightPos <= characterDiveHeight then
				character:setDiving( false )
			end
		end
	end

	-- Scaled movement slowdown when walking through water
	if not character:isSwimming() then
		local depthScale = 1 - math.max( math.min( ( ( character.worldPosition.z + characterDiveOffset ) - waterHeightPos ) / ( characterDiveOffset * 2 ), 1.0 ), 0.0 )
		character.movementSpeedFraction =  math.max( math.min( 1 - ( depthScale + 0.1 ), 1.0 ), 0.3 )
	else
		character.movementSpeedFraction = 1.0
	end

	if character:isTumbling() then
		local worldPosition = character:getTumblingWorldPosition()
		local worldRotation = character:getTumblingWorldRotation()
		local velocity = character:getTumblingLinearVelocity()
		local halfExtent = character:getTumblingExtent() * 0.5
		local mass = character:getTumblingMass()
		-- local mass = 10.0
		local force = CalulateForce( waterHeightPos, worldPosition, worldRotation, velocity, halfExtent, mass )
		character:applyTumblingImpulse( force )
	else
		-- Push up if under surface
		local waterHeightFloatThreshold = waterHeightPos - character:getHeight() * 0.5
		if not character:getCanSwim()  then
			local characterForce = sm.vec3.new( 0, 0, character:getMass() * 15 )
			sm.physics.applyImpulse( character, characterForce * 0.025, true )
		elseif ( characterFloatHeight < waterHeightPos and characterFloatHeight > waterHeightFloatThreshold ) then
			-- Buoyancy force formula
			local gravity = 10
			local fluidDensity = 1000
			local displacedVolume = 0.0664
			local buoyancyForce = fluidDensity * displacedVolume * gravity
			local diveDepthScale = 1 - math.max( math.min( ( characterFloatHeight - waterHeightFloatThreshold ) / ( waterHeightPos - waterHeightFloatThreshold ) , 1.0 ), 0.0 )
			local characterForce = sm.vec3.new( 0, 0, buoyancyForce * diveDepthScale )
			sm.physics.applyImpulse( character, characterForce * 0.025, true )
		end
	end
end

----------------------------------------------------------------------------------------------------

function WaterManager.trigger_onEnterWater( self, trigger, results )

	local ud = trigger:getUserData()
	assert(ud)
	for _,result in ipairs( results ) do
		if sm.exists( result ) then
			if type( result ) == "Character" then
				local triggerMax = trigger:getWorldMax()
				local characterPos = result:getWorldPosition()
				local splashPosition = sm.vec3.new( characterPos.x, characterPos.y, triggerMax.z )

				if not result:isSwimming() then
					PlaySplashEffect( splashPosition, result:getVelocity(), result:getMass() )
				end

				-- Only trigger once per tick
				if self.triggeredCharacters[result.id] == nil then
					self.triggeredCharacters[result.id] = true

					UpdateCharacterInWater( trigger, result )

					if sm.isHost then
						if result:isPlayer() and ud.chemical then
							sm.event.sendToPlayer( result:getPlayer(), "sv_e_onEnterChemical" )
						end
					end
				end

			elseif type( result ) == "Body" then
				local triggerMax = trigger:getWorldMax()
				local centerPos = result:getCenterOfMassPosition()
				local splashPosition = sm.vec3.new( centerPos.x, centerPos.y, triggerMax.z )
				PlaySplashEffect( splashPosition, result:getVelocity(), result:getMass() )
			end
		end
	end
end

function WaterManager.trigger_onExitWater( self, trigger, results )

	local ud = trigger:getUserData()
	assert(ud)
	for _, result in ipairs( results ) do
		if sm.exists( result ) then
			if type( result ) == "Character" then
				if sm.isHost then
					if result:isSwimming() then
						result:setSwimming( false )
					end
					if result:isDiving() then
						result:setDiving( false )
					end

					result.movementSpeedFraction = 1.0
					
					if sm.isHost then
						if result:isPlayer() and ud.chemical then
							sm.event.sendToPlayer( result:getPlayer(), "sv_e_onExitChemical" )
						end
					end
				end
			end
		end
	end
end
WaterManager.points = {}
function WaterManager.trigger_onStayWater( self, trigger, results )

	local ud = trigger:getUserData()
	assert(ud)
	for _, result in ipairs( results ) do
		if sm.exists( result ) then
			if type( result ) == "Character" then

				-- Only trigger once per tick
				if self.triggeredCharacters[result.id] == nil then
					self.triggeredCharacters[result.id] = true

					UpdateCharacterInWater( trigger, result )

					if sm.isHost then
						if result:isPlayer() and ud.chemical then
							sm.event.sendToPlayer( result:getPlayer(), "sv_e_onStayChemical" )
						end
					end
				end
			elseif type( result ) == "Body" then

				-- Only trigger once per tick
				if self.triggeredBodies[result.id] == nil then
					self.triggeredBodies[result.id] = true

					local body = result
					local waterHeightPos = trigger:getWorldMax().z
					local angular_velocity = body:getAngularVelocity()

					for _, shape in pairs( body:getShapes() ) do
						local shapeBounds = shape:getBoundingBox()
						local shapeVolume = shapeBounds.x * shapeBounds.y * shapeBounds.z
						local shapeRotation = shape.worldRotation
						local shapeCornerOffset = shape:getBoundingBox() * 0.5

						-- Setup all the corners of the shape's bounding box
						local corners = {}
						corners[1] = shape.worldPosition + shapeRotation * sm.vec3.new( shapeCornerOffset.x, shapeCornerOffset.y, shapeCornerOffset.z )
						corners[2] = shape.worldPosition + shapeRotation * sm.vec3.new( shapeCornerOffset.x, shapeCornerOffset.y, -shapeCornerOffset.z )
						corners[3] = shape.worldPosition + shapeRotation * sm.vec3.new( shapeCornerOffset.x, -shapeCornerOffset.y, shapeCornerOffset.z )
						corners[4] = shape.worldPosition + shapeRotation * sm.vec3.new( shapeCornerOffset.x, -shapeCornerOffset.y, -shapeCornerOffset.z )
						corners[5] = shape.worldPosition + shapeRotation * sm.vec3.new( -shapeCornerOffset.x, shapeCornerOffset.y, shapeCornerOffset.z )
						corners[6] = shape.worldPosition + shapeRotation * sm.vec3.new( -shapeCornerOffset.x, shapeCornerOffset.y, -shapeCornerOffset.z )
						corners[7] = shape.worldPosition + shapeRotation * sm.vec3.new( -shapeCornerOffset.x, -shapeCornerOffset.y, shapeCornerOffset.z )
						corners[8] = shape.worldPosition + shapeRotation * sm.vec3.new( -shapeCornerOffset.x, -shapeCornerOffset.y, -shapeCornerOffset.z )
						local VelocityNorm = sm.vec3.new(0,0,0)
						if shape.velocity.x >= 0 then VelocityNorm.x = 1 else VelocityNorm.x = - 1 end
						if shape.velocity.y >= 0 then VelocityNorm.y = 1 else VelocityNorm.y = - 1 end
						if shape.velocity.z >= 0 then VelocityNorm.z = 1 else VelocityNorm.z = - 1 end
						local VelocityIndex = sm.vec3.new(VelocityNorm.x, VelocityNorm.y, VelocityNorm.z)
						local set = { }
						-- normalized points addigned to corresponding corners
						set[sm.vec3.new(1,1,1)] = corners[1]
						set[sm.vec3.new(1,1,-1)] = corners[2]
						set[sm.vec3.new(1,-1,1)] = corners[3]
						set[sm.vec3.new(1,-1,-1)] = corners[4]
						set[sm.vec3.new(-1,1,1)] = corners[5]
						set[sm.vec3.new(-1,1,-1)] = corners[6] 
						set[sm.vec3.new(-1,-1,1)] = corners[7]
						set[sm.vec3.new(-1,-1,-1)] = corners[8] 
						-- constant points assigned to normalized vector corners
						local start = set[GetValue(VelocityIndex, set)]
						local Xdiff = set[GetValue(VelocityIndex*sm.vec3.new(-1,1,1), set)]
						local Ydiff = set[GetValue(VelocityIndex*sm.vec3.new(1,-1,1), set)]
						local Zdiff = set[GetValue(VelocityIndex*sm.vec3.new(1,1,-1), set)]
						local XZdiff = set[GetValue(VelocityIndex*sm.vec3.new(-1,1,-1), set)]
						local YZdiff = set[GetValue(VelocityIndex*sm.vec3.new(1,-1,-1), set)]
						--Face-1-Surface-Area-Cacluations
						-- slope calculations
						local f1_up_slope = ((waterHeightPos - start.z) / (start-Xdiff):normalize().z)
						local f1_up_highpoint
						-- deternmines the maximum point and uses the slope to calcualte the point where the side of the face will reach the surface
						if f1_up_slope ~= 1/0 and f1_up_slope ~= -1/0 then 
							local point = start + ((start-Xdiff):normalize() * f1_up_slope) 
							if (point.z <= Xdiff.z and point.z >= start.z) or (point.z >= Xdiff.z and point.z <= start.z) then 
								f1_up_highpoint = point 
							else 
								f1_up_highpoint = Xdiff 
							end 
						else 
							f1_up_highpoint = Xdiff 
						end
						-- slope calculations
						local f1_down_slope = ((waterHeightPos - Zdiff.z) / (Zdiff-XZdiff):normalize().z)
						local f1_down_highpoint
						-- deternmines the maximum point and uses the slope to calcualte the point where the side of the face will reach the surface
						if f1_down_slope ~= 1/0 and f1_down_slope ~= -1/0 then 
							local point = Zdiff + ((Zdiff-XZdiff):normalize() * f1_down_slope) 
							if (point.z <= XZdiff.z and point.z >= Zdiff.z) or (point.z >= XZdiff.z and point.z <= Zdiff.z) then 
								f1_down_highpoint = point 
							else 
								f1_down_highpoint = XZdiff 
							end 
						else 
							f1_down_highpoint = XZdiff 
						end
						-- calcualtes the difference between the top corner and the top left corner and the bottom and bottom left corner ( relativally speaking )
						local f1_bottom_diff = (f1_down_highpoint - Zdiff)
						local f1_up_diff = (f1_up_highpoint - start)
						-- calcualtes the difference between the top corner and the bottom corner ( relativally speaking )
						local f1_z_diff = math.pow(math.pow((start-Zdiff).x, 2) + math.pow((start-Zdiff).y, 2) + math.pow((start-Zdiff).z, 2), 1/2)
						-- if the top corner Z is above sea level for what ever reason, but Zdiff id below sea level
						if start.z > waterHeightPos then
							f1_z_diff = waterHeightPos - Zdiff.z
						end
						-- uses previouly calcuated a and b lengths (a2+b2) to get c2 then multiplying that by the difference between the start and ZDiff
						local f1_bottom_sa = (math.pow(math.pow(f1_bottom_diff.x, 2) + math.pow(f1_bottom_diff.y, 2) + math.pow(f1_bottom_diff.z, 2), 1/2) * 4 ) * (f1_z_diff * 4)
						local f1_up_sa = (math.pow(math.pow(f1_up_diff.x, 2) + math.pow(f1_up_diff.y, 2) + math.pow(f1_up_diff.z, 2), 1/2) * 4 ) * (f1_z_diff * 4)
						-- uses two points of calcualtions the top length and the bottom length and avarages them
						local f1_surface_area = math.abs((f1_bottom_sa + f1_up_sa)/2)
						-- calcualtes the point at which to apply the water resistance force relative to the center of the surface area
						local f1_force_point = ((f1_up_highpoint - start)/2) + start
						--Face-2-Surface-Area-Cacluations
						--slope calcualtions slope
						local f2_up_slope = ((waterHeightPos - start.z) / (start-Ydiff):normalize().z)
						local f2_up_highpoint 
						-- deternmines the maximum point and uses the slope to calcualte the point where the side of the face will reach the surface
						if f2_up_slope ~= 1/0 and f2_up_slope ~= -1/0 then 
							local point = start + ((start-Ydiff):normalize() * f2_up_slope) 
							if (point.z <= Ydiff.z and point.z >= start.z) or (point.z >= Ydiff.z and point.z <= start.z) then 
								f2_up_highpoint = point 
							else 
								f2_up_highpoint = Ydiff 
							end 
						else 
							f2_up_highpoint = Ydiff 
						end
						--slope calcualtions slope
						local f2_down_slope = ((waterHeightPos - Zdiff.z) / (Zdiff-YZdiff):normalize().z)
						local f2_down_highpoint
						-- deternmines the maximum point and uses the slope to calcualte the point where the side of the face will reach the surface
						if f2_down_slope ~= 1/0 and f2_down_slope ~= -1/0 then 
							local point = Zdiff + ((Zdiff-YZdiff):normalize() * f2_down_slope) 
							if (point.z <= YZdiff.z and point.z >= Zdiff.z) or (point.z >= YZdiff.z and point.z <= Zdiff.z) then 
								f2_down_highpoint = point 
							else 
								f2_down_highpoint = YZdiff 
							end 
						else 
							f2_down_highpoint = YZdiff
						end
						-- calcualtes the difference between the top corner and the top left corner and the bottom and bottom left corner ( relativally speaking )
						local f2_bottom_diff = (f2_down_highpoint - Zdiff)
						local f2_up_diff = (f2_up_highpoint - start)
						-- calcualtes the difference between the top corner and the bottom corner ( relativally speaking )
						local f2_z_diff = math.pow(math.pow((start-Zdiff).x, 2) + math.pow((start-Zdiff).y, 2) + math.pow((start-Zdiff).z, 2), 1/2)
						-- if the top corner Z is above sea level for what ever reason, but Zdiff id below sea level
						if start.z > waterHeightPos then
							f2_z_diff = waterHeightPos - Zdiff.z
						end
						-- uses previouly calcuated a and b lengths (a2+b2) to get c2 then multiplying that by the difference between the start and ZDiff
						local f2_bottom_sa = (math.pow(math.pow(f2_bottom_diff.x, 2) + math.pow(f2_bottom_diff.y, 2) + math.pow(f2_bottom_diff.z, 2), 1/2) * 4 ) * (f2_z_diff * 4)
						local f2_up_sa = (math.pow(math.pow(f2_up_diff.x, 2) + math.pow(f2_up_diff.y, 2) + math.pow(f2_up_diff.z, 2), 1/2) * 4 ) * (f2_z_diff * 4)
						-- uses two points of calcualtions the top length and the bottom length and avarages them
						local f2_surface_area = math.abs((f2_bottom_sa + f2_up_sa)/2)
						-- calcualtes the point at which to apply the water resistance force relative to the center of the surface area
						local f2_force_point = ((f2_up_highpoint - start)/2) + start
						--Face-3-Surface-Area-Cacluations
						-- rough cacluations for face 3
						local f3_surface_area = math.abs( ( f2_up_sa/f2_z_diff ) * ( f1_up_sa/f1_z_diff) ) / 16
						local f3_force_point
						if VelocityNorm.z == 1 then
							f3_force_point = ((f2_up_highpoint - f1_up_highpoint) / 2) + f1_up_highpoint
						else
							f3_force_point = ((f2_down_highpoint - f1_down_highpoint) / 2) + f1_down_highpoint
						end
						----------------------------------------------------------
						-- Surface Area stuff
						local total_surface_area = f1_surface_area + f2_surface_area + f3_surface_area
						local f1_sa_percent_calc = f1_surface_area / total_surface_area
						local f2_sa_percent_calc = f2_surface_area / total_surface_area
						local f3_sa_percent_calc = f3_surface_area / total_surface_area
						local velocityNormal
						-- capps length of velocity
						local cappedVeloity = sm.vec3.new(math.floor(shape.velocity.x*100)/100, math.floor(shape.velocity.y*100)/100, math.floor(shape.velocity.z*100)/100 )
						if math.abs(cappedVeloity:length()) > 0 then
							velocityNormal = shape.velocity:normalize()
						else
							velocityNormal = sm.vec3.new(0,0,0)
						end
						-- finds difference between the normalized velocity and the "normalized" velocity (fixed to 1 or -1 on x, y, and z as a reference to each side, +x, -x, +y, -y, +z, -z)
						local x =  ( math.floor( ( VelocityNorm.x - velocityNormal.x - 1 ) * 100 ) / 100 )
						local y =  ( math.floor( ( VelocityNorm.y - velocityNormal.y - 1 ) * 100 ) / 100 )
						local z =  ( math.floor( ( VelocityNorm.z - velocityNormal.z - 1 ) * 100 ) / 100 )
						-- prevents inf when dividing 0 by 100
						if math.abs(x) == 1/0 then x = 1 end
						if math.abs(y) == 1/0 then y = 1 end
						if math.abs(z) == 1/0 then z = 1 end
						-- takes the abs of the difference and subtracts a constant
						-- this is used for multiplying the velocity to get the proper anti velocity
						local offesets = sm.vec3.new( math.abs(x) - 0.1 , math.abs(y) - 0.1 , math.abs(z) - 0.1 )
						-- Sort so the lowest corners go first
						table.sort ( corners, function ( c1, c2 ) return c1.z < c2.z end )
						-- Check which of the lowest corners are under water and calculate their total depth

						-- WaterManager.points can be used for calcualting the difference between the corners previous position and current position
						-- previousPoints[1-8] return the difference between the corners previous position and current position
						local submergedPoints = {}
						local previousPoints = {}
						local totalDepth = 0
						local avgDepth = 0
						for _, corner in pairs( { corners[1], corners[2], corners[3], corners[4] } ) do
							if corner.z < waterHeightPos then
								totalDepth = totalDepth + ( waterHeightPos - corner.z )
								submergedPoints[#submergedPoints+1] = corner
								local changeInPos = sm.vec3.new(0,0,0)
								if WaterManager.points[shape.id] == nil then
									WaterManager.points[shape.id] = { corners = {}, center = {} }
								else
									changeInPos = shape.worldPosition - WaterManager.points[shape.id].center
								end
								if WaterManager.points[shape.id].corners[#submergedPoints] ~= nil then
									previousPoints[#previousPoints+1] = corner - (WaterManager.points[shape.id].corners[#submergedPoints] + changeInPos)
								end
								WaterManager.points[shape.id].corners[#submergedPoints] = corner
								WaterManager.points[shape.id].center = shape.worldPosition
							end
						end
						avgDepth = totalDepth * 0.25
						-- calcualtes water resistance for x, y and z using prevously calcualted variables
						local VEl = -(offesets * shape.velocity ) * 8
						VEl.x = math.floor( VEl.x * 100 ) / 100
						VEl.y = math.floor( VEl.y * 100 ) / 100
						VEl.z = math.floor( VEl.z * 100 ) / 100
						-- applies water resistance inpulses
						if tostring(math.abs(f1_sa_percent_calc)) ~= "nan" then
							sm.physics.applyImpulse( shape, VEl * f1_sa_percent_calc * sm.vec3.new(1,0,0), false, f1_force_point - shape.worldPosition )
							sm.physics.applyImpulse( shape, VEl * f2_sa_percent_calc * sm.vec3.new(0,1,0), false, f2_force_point - shape.worldPosition )
							sm.physics.applyImpulse( shape, VEl * f3_sa_percent_calc * sm.vec3.new(0,0,1), false, f3_force_point - shape.worldPosition )
						end

						-- Approximate how much of the shape is submerged to calculate the displaced volume
						local bottomArea = 0.5 * ( ( corners[1] - corners[2] ):cross( corners[1] - corners[3] ):length() + ( corners[4] - corners[2] ):cross( corners[4] - corners[3] ):length() )
						local displacedVolume = math.min( ( bottomArea * avgDepth ), shapeVolume )

						-- Buoyancy force formula
						local gravity = 10
						local fluidDensity = 1000
						local buoyancyForce = fluidDensity * displacedVolume * gravity
						buoyancyForce = buoyancyForce * shape:getBuoyancy()

						-- Apply buoyancy force to the shape's body using the shape's CoM as an offset
						local shapeForce = sm.vec3.new( 0, 0, buoyancyForce )
						sm.physics.applyImpulse( shape, shapeForce * 0.025, true )

						-- Diminishing force in water, acts as water resistance
						if shape.velocity:length() >= FLT_EPSILON then
							local direction = shape.velocity:normalize()
							local magnitude = shape.velocity:length()

							local antiVelocity = -direction * ( magnitude * 0.15 )
							if shape.velocity.z > 0 then
								antiVelocity = -direction * ( magnitude * 0.15 ) --Ascending
							end
							local antiForceVector = ( antiVelocity / 0.025 ) * shape.mass

							local horizontalDiminishScale = 0.1
							antiForceVector.x = antiForceVector.x * horizontalDiminishScale
							antiForceVector.y = antiForceVector.y * horizontalDiminishScale
							sm.physics.applyImpulse( shape, antiForceVector * 0.01, true )
						end

						-- Optional, apply distributed impulses on each corner of the shape scaled by depth
						--for _, point in pairs( submergedPoints ) do
						--	--local relativePosition = point - shape.worldPosition
						--	local relativePosition = shape:transformPoint( point )
						--	local forceDirection = sm.vec3.new( 0, 0, buoyancyForce * ( ( waterHeightPos - point.z ) / totalDepth ) )
						--	local force = sm.vec3.new( forceDirection:dot( shape.right ), forceDirection:dot( shape.at ), forceDirection:dot( shape.up ) )
						--	sm.physics.applyImpulse( shape, force * 0.025 * 0.8, false, relativePosition  )
						--	--sm.physics.applyImpulse( shape, force * 0.025, false, relativePosition )
						--end

					end
				end
			end
		end
	end

end

function WaterManager.trigger_onProjectile( self, trigger, hitPos, hitTime, hitVelocity, projectileName )

	sm.effect.playEffect( "Projectile - HitWater", hitPos )
	return false
end

function GetValue( v, table )
	if table[v] ~= nil then
		return v
	else
		for k,h in  pairs(table) do
			if v == k then
				return k
			elseif v == h then
				return h
			end
		end
	end
	return nil
end
