dofile( "$SURVIVAL_DATA/Scripts/game/survival_constants.lua" )
dofile( "$SURVIVAL_DATA/Scripts/util.lua" )

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

function trunc(x)
	return math.floor(math.floor(x * 1000) + 0.5) / 1000
end

function WaterVelocityCheck(vel)
	local peakFac = 368 / 45

	if vel.x ~= vel.x or vel.y ~= vel.y or vel.z ~= vel.z then return sm.vec3.zero() end
	local absVel = sm.vec3.new(
		trunc(math.abs(vel.x)),
		trunc(math.abs(vel.y)),
		trunc(math.abs(vel.z))
	)

	local resistance = {
		x = trunc((math.pow(absVel.x, 3)/3)+3),
		y = trunc((math.pow(absVel.y, 3)/3)+3),
		z = trunc((math.pow(absVel.z, 3)/3)+3)
	}

	local returnVec = sm.vec3.zero()

	if math.abs(resistance.x) > 0.001
		then returnVec.x = ((peakFac*absVel.x) / resistance.x)+1 end
	if math.abs(resistance.y) > 0.001
		then returnVec.y = ((peakFac*absVel.y) / resistance.y)+1 end
	if math.abs(resistance.z) > 0.001
		then returnVec.z = ((peakFac*absVel.z) / resistance.z)+1 end
	
	return returnVec * vel
end

function getSubmergedVolume(minPt, maxPt, z_limit, volume)
    -- Check if the body is not submerged at all
    if minPt.z > z_limit then
        return 0
    end

    -- Check if the body is fully submerged
    if maxPt.z <= z_limit then
        return volume
    end

    -- Otherwise, the body is partially submerged and we need to calculate the submerged volume
    -- We can create a new AABB representing the submerged part by clamping the max point to the z_limit
    local submergedMaxPt = sm.vec3.new(maxPt.x, maxPt.y, math.min(maxPt.z, z_limit))

    -- Calculate the dimensions of the submerged part
    local submergedDimensions = submergedMaxPt - minPt

    -- The volume of the submerged part is then the product of the dimensions
    return submergedDimensions.x * submergedDimensions.y * submergedDimensions.z * 32
end

function truncVec(vec)
	return sm.vec3.new(
		math.abs(math.floor(vec.x*1000)/1000),
		math.abs(math.floor(vec.y*1000)/1000),
		math.abs(math.floor(vec.z*1000)/1000))
end
  
function calculateExposure(normal, velocity)
	if truncVec(normal):length() <= 0.01 or truncVec(velocity):length() <= 0.01 then return 0 end
    local normNormal = normal:normalize()
    local normVelocity = velocity:normalize()
    local cosAngle = normNormal:dot(normVelocity)
    local exposure = 1 - cosAngle

    -- Calculate dot product to determine if the face is in the direction of the velocity
    local dotProduct = normNormal:dot(normVelocity)

    -- If the face is in the direction of the velocity
    if dotProduct > 0 then
        return exposure
    else
        return exposure * 0.05 -- 5% of the exposure for water cavitation
    end
end

function getFaces(vertices)
    local faces = {}
    if #vertices == 6 then
        -- Triangular prism: each face is a unique combination of 3 vertices
        for i = 1, #vertices - 2 do
            for j = i + 1, #vertices - 1 do
                for k = j + 1, #vertices do
                    table.insert(faces, {vertices[i], vertices[j], vertices[k]})
                end
            end
        end
    elseif #vertices == 8 then
        -- Rectangular prism: each face is a unique combination of 4 vertices
        for i = 1, #vertices - 3 do
            for j = i + 1, #vertices - 2 do
                for k = j + 1, #vertices - 1 do
                    for l = k + 1, #vertices do
                        table.insert(faces, {vertices[i], vertices[j], vertices[k], vertices[l]})
                    end
                end
            end
        end
    end
    return faces
end

function FindCenterOfBuoyancy(shape, z_limit, velocity)
	local boundingBox = shape:getBoundingBox()
	local worldPosition = shape:getWorldPosition()
	local worldRotation = shape:getWorldRotation()
	local volume = boundingBox.x * 4 * boundingBox.y * 4 * boundingBox.z * 4

	-- Calculate the half extents of the bounding box
	local halfExtent = sm.vec3.new( boundingBox.x / 2.0, boundingBox.y / 2.0, boundingBox.z / 2.0 )

	-- Setup all the corners of the shape's bounding box using the bounding box half extents
	local vertices = {}
	vertices[1] = worldPosition + worldRotation * sm.vec3.new( halfExtent.x, halfExtent.y, halfExtent.z )
	vertices[2] = worldPosition + worldRotation * sm.vec3.new( halfExtent.x, halfExtent.y, -halfExtent.z )
	vertices[3] = worldPosition + worldRotation * sm.vec3.new( halfExtent.x, -halfExtent.y, halfExtent.z )
	vertices[4] = worldPosition + worldRotation * sm.vec3.new( halfExtent.x, -halfExtent.y, -halfExtent.z )
	vertices[5] = worldPosition + worldRotation * sm.vec3.new( -halfExtent.x, halfExtent.y, halfExtent.z )
	vertices[6] = worldPosition + worldRotation * sm.vec3.new( -halfExtent.x, halfExtent.y, -halfExtent.z )
	vertices[7] = worldPosition + worldRotation * sm.vec3.new( -halfExtent.x, -halfExtent.y, halfExtent.z )
	vertices[8] = worldPosition + worldRotation * sm.vec3.new( -halfExtent.x, -halfExtent.y, -halfExtent.z )

	-- Define the cube's edges
	local edges = {
		{vertices[1], vertices[2]},  -- Edge 1
		{vertices[2], vertices[3]},  -- Edge 2
		{vertices[3], vertices[4]},  -- Edge 3
		{vertices[4], vertices[1]},  -- Edge 4
		{vertices[5], vertices[6]},  -- Edge 5
		{vertices[6], vertices[7]},  -- Edge 6
		{vertices[7], vertices[8]},  -- Edge 7
		{vertices[8], vertices[5]},  -- Edge 8
		{vertices[1], vertices[5]},  -- Edge 9
		{vertices[2], vertices[6]},  -- Edge 10
		{vertices[3], vertices[7]},  -- Edge 11
		{vertices[4], vertices[8]}   -- Edge 12
	}

	-- Store vertices that are below and on the z_limit
	local below_limit_vertices = {}
	local on_limit_vertices = {}
	local combinedVertices = {}

	-- Filter vertices
	for i, vertex in ipairs(vertices) do
		if vertex.z < z_limit then
			table.insert(below_limit_vertices, vertex)
			table.insert(combinedVertices, vertex)
		elseif vertex.z == z_limit then
			table.insert(on_limit_vertices, vertex)
			table.insert(combinedVertices, vertex)
		end
	end

	--If the slicing plane is above or below the cube, print a message and stop the program
	if #below_limit_vertices == 0 then
		--print("The slicing plane is below the cube.")
		return {worldPosition, 0, sm.vec3.zero()}
	elseif #below_limit_vertices >= 8 then
		--print("The slicing plane is above the cube.")
		return {worldPosition, volume, sm.vec3.zero()}
	end

	-- Find the intersecting points
	local intersecting_vertices = {}
	for i, edge in ipairs(edges) do
		local vertex1 = edge[1]
		local vertex2 = edge[2]
		-- Check if one vertex is below and one is above the limit
		if (vertex1.z < z_limit and vertex2.z > z_limit) or (vertex1.z > z_limit and vertex2.z < z_limit) then
			-- Find the intersection point (linear interpolation)
			local t = (z_limit - vertex1.z) / (vertex2.z - vertex1.z)
			local x_intersect = vertex1.x + t * (vertex2.x - vertex1.x)
			local y_intersect = vertex1.y + t * (vertex2.y - vertex1.y)
			local nvert = sm.vec3.new(x_intersect, y_intersect, z_limit)
			table.insert(intersecting_vertices, nvert)
			table.insert(combinedVertices, nvert)
		end
	end

	-- Compute the centroid of the vertices below the limit, on the limit, and the intersecting points
	local centroid_x = 0
	local centroid_y = 0
	local centroid_z = 0
	local total_vertices = #below_limit_vertices + #on_limit_vertices + #intersecting_vertices

	for i, vertex in ipairs(below_limit_vertices) do
		centroid_x = centroid_x + vertex.x
		centroid_y = centroid_y + vertex.y
		centroid_z = centroid_z + vertex.z
	end

	for i, vertex in ipairs(on_limit_vertices) do
		centroid_x = centroid_x + vertex.x
		centroid_y = centroid_y + vertex.y
		centroid_z = centroid_z + vertex.z
	end

	for i, vertex in ipairs(intersecting_vertices) do
		centroid_x = centroid_x + vertex.x
		centroid_y = centroid_y + vertex.y
		centroid_z = centroid_z + vertex.z
	end

	centroid_x = centroid_x / total_vertices
	centroid_y = centroid_y / total_vertices
	centroid_z = centroid_z / total_vertices

	if centroid_x ~= centroid_x or centroid_y ~= centroid_y or centroid_z ~= centroid_z then return {sm.vec3.zero(), 0} end
	
	intersection_vertex = sm.vec3.new(centroid_x, centroid_y, centroid_z)

	-- Find the lowest vertecies
	table.sort(vertices, function(a, b) return a.z < b.z end)
    lowest_vertecies = {vertices[1], vertices[2], vertices[3], vertices[4]}

    local sum_x = 0
    local sum_y = 0
    local sum_z = 0
    local num_points = #lowest_vertecies

    for i=1, num_points do
        sum_x = sum_x + lowest_vertecies[i].x
        sum_y = sum_y + lowest_vertecies[i].y
        sum_z = sum_z + lowest_vertecies[i].z
    end

    lowest_vertex = sm.vec3.new(sum_x / num_points, sum_y / num_points, sum_z / num_points)

	local center_of_mass = sm.vec3.new(
	 	(lowest_vertex.x + intersection_vertex.x) / 2,
	 	(lowest_vertex.y + intersection_vertex.y) / 2,
		(lowest_vertex.z + intersection_vertex.z) / 2
	)

	local vertexFactor = (#below_limit_vertices + #on_limit_vertices) / 8
	local MaxHeight = (boundingBox.z / 2)
	local MaxHeightX = (boundingBox.x / 2)
	local MaxHeightY = (boundingBox.y / 2)
	if MaxHeightY > MaxHeight then
		MaxHeight = MaxHeightY
	end
	if MaxHeightX > MaxHeight then
		MaxHeight = MaxHeightX
	end
	local gFactor = ((MaxHeight / math.abs(worldPosition.z - center_of_mass.z)) / MaxHeight) - 1
	if gFactor > 1 then
		gFactor = 2 - gFactor
	end
	if gFactor < 0 then gFactor = 1 end
	local displacedVolume = 0
	if worldPosition.z + 0.1 > z_limit then
		displacedVolume = math.floor(math.min(gFactor * volume, volume * vertexFactor)+0.5)
	else
		displacedVolume = math.floor((gFactor * volume)+0.5)
	end

	displacedVolume = math.min(displacedVolume, volume)
	if worldPosition.z > z_limit then displacedVolume = math.min(displacedVolume, volume * (0.5 * (displacedVolume / volume))) end

	-- Get the faces from the combined vertices
	-- local faces = getFaces(combinedVertices)
	local resistanceVector = sm.vec3.zero()
	-- Loop through each face
	-- for _, face in ipairs(faces) do
	-- 	-- Calculate the surface normal. Assuming the face vertices are in a consistent order (right-hand rule).
	-- 	local edge1 = face[2] - face[1]
	-- 	local edge2 = face[3] - face[2]
	-- 	local normal = edge1:cross(edge2)

	-- 	-- Calculate the exposure
	-- 	local exposure = calculateExposure(normal, velocity)

	-- 	resistanceVector = resistanceVector + (normal * exposure)
	-- end
	return {center_of_mass, displacedVolume, resistanceVector}
end


-- ################################ World cell callbacks ################################

-- Loaded/Reloaded

function WaterManager.onCellLoaded( self, x, y )
	--print("--- loading water objs on cell " .. x .. ":" .. y .. " ---")
	local cellKey = CellKey(x, y)
	local waterNodes = sm.cell.getNodesByTag( x, y, "WATER" )
	local chemicalNodes = sm.cell.getNodesByTag( x, y, "CHEMICALS" )
	local oilNodes = sm.cell.getNodesByTag( x, y, "OIL" )

	if #waterNodes > 0 or #chemicalNodes > 0 or #oilNodes > 0 then
		self.cells[cellKey] = {}
	end

	local idx = 1
	if #waterNodes > 0 then
		for _, node in ipairs( waterNodes ) do
			local areaTrigger = sm.areaTrigger.createBox( node.scale * 0.5, node.position, node.rotation, sm.areaTrigger.filter.character + sm.areaTrigger.filter.dynamicBody + sm.areaTrigger.filter.areaTrigger, { water = true } )
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
			local areaTrigger = sm.areaTrigger.createBox( node.scale * 0.5, node.position, node.rotation, sm.areaTrigger.filter.character + sm.areaTrigger.filter.dynamicBody + sm.areaTrigger.filter.areaTrigger, { chemical = true } )
			areaTrigger:bindOnEnter( "trigger_onEnterWater", self )
			areaTrigger:bindOnExit( "trigger_onExitWater", self )
			areaTrigger:bindOnStay( "trigger_onStayWater", self )
			areaTrigger:bindOnProjectile( "trigger_onProjectile", self )
			self.cells[cellKey][idx] = areaTrigger
			idx = idx + 1
		end
	end

	if #oilNodes > 0 then
		for _, node in ipairs( oilNodes ) do
			local areaTrigger = sm.areaTrigger.createBox( node.scale * 0.5, node.position, node.rotation, sm.areaTrigger.filter.character + sm.areaTrigger.filter.dynamicBody + sm.areaTrigger.filter.areaTrigger, { oil = true } )
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
	if self.anim == nil then
		self.anim = sm.effect.createEffect( "ShapeRenderable" )
		self.anim:setParameter("uuid", sm.uuid.new("220b201e-aa40-4995-96c8-e6007af160de"))
		self.anim:setScale(sm.vec3.one() * 0.27)
		self.anim:start()
	end
	if self.animPos ~= nil then
		self.anim:setPosition(self.animPos + sm.vec3.new(0,0,0))
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
	-- Setup all the vertices of the shape's bounding box
	local volume = ( halfExtent.x * 2.0 * halfExtent.y * 2.0 * halfExtent.z * 2.0 )

	local vertices = {}
	vertices[1] = worldPosition + worldRotation * sm.vec3.new( halfExtent.x, halfExtent.y, halfExtent.z )
	vertices[2] = worldPosition + worldRotation * sm.vec3.new( halfExtent.x, halfExtent.y, -halfExtent.z )
	vertices[3] = worldPosition + worldRotation * sm.vec3.new( halfExtent.x, -halfExtent.y, halfExtent.z )
	vertices[4] = worldPosition + worldRotation * sm.vec3.new( halfExtent.x, -halfExtent.y, -halfExtent.z )
	vertices[5] = worldPosition + worldRotation * sm.vec3.new( -halfExtent.x, halfExtent.y, halfExtent.z )
	vertices[6] = worldPosition + worldRotation * sm.vec3.new( -halfExtent.x, halfExtent.y, -halfExtent.z )
	vertices[7] = worldPosition + worldRotation * sm.vec3.new( -halfExtent.x, -halfExtent.y, halfExtent.z )
	vertices[8] = worldPosition + worldRotation * sm.vec3.new( -halfExtent.x, -halfExtent.y, -halfExtent.z )
	
	-- Sort so the lowest vertices go first
	table.sort( vertices, function ( c1, c2 ) return c1.z < c2.z end )

	-- Check which of the lowest vertices are under water and calculate their total depth
	local submergedPoints = {}
	local totalDepth = 0
	local avgDepth = 0
	for _, corner in pairs( { vertices[1], vertices[2], vertices[3], vertices[4] } ) do
		if corner.z < waterHeightPos then
			totalDepth = totalDepth + ( waterHeightPos - corner.z )
			submergedPoints[#submergedPoints+1] = corner
		end
	end
	avgDepth = totalDepth * 0.25

	-- Approximate how much of the shape is submerged to calculate the displaced volume
	local bottomArea = 0.5 * ( ( vertices[1] - vertices[2] ):cross( vertices[1] - vertices[3] ):length() + ( vertices[4] - vertices[2] ):cross( vertices[4] - vertices[3] ):length() )
	local displacedVolume = math.min( ( bottomArea * avgDepth ), volume )

	-- Buoyancy force formula
	local fluidDensity = 500 -- density level 4
	local buoyancyForce = fluidDensity * displacedVolume * GRAVITY

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
	local waterMovementSpeedFraction = 1.0
	if not character:isSwimming() then
		local depthScale = 1 - math.max( math.min( ( ( character.worldPosition.z + characterDiveOffset ) - waterHeightPos ) / ( characterDiveOffset * 2 ), 1.0 ), 0.0 )
		waterMovementSpeedFraction = math.max( math.min( 1 - ( depthScale + 0.1 ), 1.0 ), 0.3 )
	end
	if sm.isHost then
		if character.publicData then
			character.publicData.waterMovementSpeedFraction = waterMovementSpeedFraction
		end
	else
		if character.clientPublicData then
			character.clientPublicData.waterMovementSpeedFraction = waterMovementSpeedFraction
		end
	end

	if character:isTumbling() then
		local worldPosition = character:getTumblingWorldPosition()
		local worldRotation = character:getTumblingWorldRotation()
		local velocity = character:getTumblingLinearVelocity()
		local halfExtent = character:getTumblingExtent() * 0.5
		local mass = character:getMass()
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
			local fluidDensity = 1000
			local displacedVolume = 0.0664
			local buoyancyForce = fluidDensity * displacedVolume * GRAVITY
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
						elseif result:isPlayer() and ud.oil then
							sm.event.sendToPlayer( result:getPlayer(), "sv_e_onEnterOil" )
						end
					end
				end

			elseif type( result ) == "Body" then
				local triggerMax = trigger:getWorldMax()
				local centerPos = result:getCenterOfMassPosition()
				local splashPosition = sm.vec3.new( centerPos.x, centerPos.y, triggerMax.z )
				PlaySplashEffect( splashPosition, result:getVelocity(), result:getMass() )
				for _,shape in ipairs(result:getShapes()) do
					local vel = shape:getVelocity()
					local data = FindCenterOfBuoyancy(shape, trigger:getWorldMax().z, vel)
					local pos, displacedVolume = data[1], data[2]

					if displacedVolume >= 1 then
						if vel:length() > 5 then
							entry_power = WaterVelocityCheck(vel) * (shape:getMass() * 0.25) * sm.vec3.new(0.3,0.3,1)
							sm.physics.applyImpulse( shape, -entry_power, true )
						end
					end
				end
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

					if sm.isHost then
						if result.publicData then
							result.publicData.waterMovementSpeedFraction = 1.0
						end
						if result:isPlayer() and ud.chemical then
							sm.event.sendToPlayer( result:getPlayer(), "sv_e_onExitChemical" )
						elseif result:isPlayer() and ud.oil then
							sm.event.sendToPlayer( result:getPlayer(), "sv_e_onExitOil" )
						end
					else
						if result.clientPublicData then
							result.clientPublicData.waterMovementSpeedFraction = 1.0
						end
					end
				end
			end
		end
	end
end

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
			elseif type(result) == "Body" then
				local vel = result:getVelocity()
				local shapes = result:getShapes()
				
				local angVel = result:getAngularVelocity()
				local angularResistance = WaterVelocityCheck(angVel) * (result:getMass() * 0.025) * 0.25
				if angularResistance:length() > 0.01 then
					sm.physics.applyTorque(result, -angularResistance, true)
				end

				for _, shape in ipairs(shapes) do
					local shapePos = shape:getWorldPosition()
					local box = shape:getBoundingBox()
					local mass = shape:getMass()
					local volume = box.x * 4 * box.y * 4 * box.z * 4
                    local data = FindCenterOfBuoyancy(shape, trigger:getWorldMax().z, vel)
					local pos, displacedVolume, expirimental = data[1], data[2], data[3]
					self.animPos = pos
					if displacedVolume >= 1 then

						local buoyancy = math.max((((shape:getBuoyancy() * (2/3))+0.2) * displacedVolume * 9.8) - mass, volume+1)
						local nLocalPos = sm.vec3.zero()
						--if shape.getIsStackable(shape:getShapeUuid()) then
						--	nLocalPos = shape:getClosestBlockLocalPosition(pos)
						--	pos = shape:transformLocalPoint( nLocalPos )
						--else
						nLocalPos = shape:transformPoint(pos)
						--end
						local resistance = WaterVelocityCheck(vel) * (mass * 0.025)
						
						-- nLocalPos = ((shape:getLocalPosition() + (box * 0.5)) * 0.25) - nLocalPos

						-- if math.abs(pos.z - trigger:getWorldMax().z) < 0.125 then nLocalPos = sm.vec3.zero() end
						if displacedVolume / volume < 0.01 then nLocalPos = sm.vec3.zero() end

						if math.abs(nLocalPos.x) < 0.125 then nLocalPos.x = 0 end
						if math.abs(nLocalPos.y) < 0.125 then nLocalPos.y = 0 end
						if math.abs(nLocalPos.z) < 0.125 then nLocalPos.z = 0 end

						sm.physics.applyImpulse(
							shape,
							sm.vec3.new(0,0,buoyancy * 0.7) - resistance + (angVel:rotate(math.pi/2, sm.vec3.new(0,0,-1)) * (mass / #shapes) * 0.25),
							true,
							nLocalPos)

						sm.physics.applyImpulse(
							shape,
							sm.vec3.new(0,0,buoyancy * 0.3),
							true)
					end
                end
            end 
		end
	end
end

function WaterManager.trigger_onProjectile( self, trigger, hitPos, hitTime, hitVelocity, _, attacker, damage, userData, hitNormal, projectileUuid )

	sm.effect.playEffect( "Projectile - HitWater", hitPos )
	return false
end
