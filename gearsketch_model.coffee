# By Frank Leenaars
# University of Twente - Department of Instructional Technology
"use strict"

# TODO:
# - point class with util methods
# - fix crashing bugs when dragging gear from chain (possibly indirectly)

# imports
Point = window.gearsketch.Point
Util = window.gearsketch.Util

window.gearsketch.model = {}

# ---------------------------
# ---------- Gear -----------
# ---------------------------
class Gear
  nextGearId = 0

  constructor: (@location, @rotation, @numberOfTeeth, id) ->
    if id?
      @id = id
    else
      @id = nextGearId.toString()
      nextGearId++
    @pitchRadius = Util.MODULE * (0.5 * @numberOfTeeth)
    @innerRadius = Util.MODULE * (0.5 * @numberOfTeeth - 1.25)
    @outerRadius = Util.MODULE * (0.5 * @numberOfTeeth + 1)

  clone: ->
    new Gear(@location.clone(), @rotation, @numberOfTeeth, @id)

window.gearsketch.model.Gear = Gear


# ---------------------------
# ------- ArcSegment --------
# ---------------------------
class ArcSegment
  constructor: (@center, @radius, @start, @end, @direction) ->

  getLength: ->
    angle = if @direction is Util.Direction.CLOCKWISE
      Util.mod(@end - @start, 2 * Math.PI)
    else
      Util.mod(@start - @end, 2 * Math.PI)
    angle * @radius

  findPoint: (distanceToGo) ->
    angleToGo = distanceToGo / @radius
    angle = @start + (if @direction is Util.Direction.CLOCKWISE then angleToGo else -angleToGo)
    @center.plus(Point.polar(angle, @radius))

  doesArcContainAngle: (angle) ->
    if @direction is Util.Direction.CLOCKWISE
      Util.mod(@end - @start, 2 * Math.PI) > Util.mod(angle - @start, 2 * Math.PI)
    else
      Util.mod(@start - @end, 2 * Math.PI) > Util.mod(@start - angle, 2 * Math.PI)

  getDistanceToPoint: (point) ->
    angle = Math.atan2(point.y - @center.y, point.x - @center.x)
    if @doesArcContainAngle(angle)
      Math.abs(point.distance(@center) - @radius)
    else
      startPoint = @center.plus(Point.polar(@start, @radius))
      endPoint = @center.plus(Point.polar(@end, @radius))
      Math.min(point.distance(startPoint), point.distance(endPoint))

  # TODO: find distance analytically
  getDistanceToSegment: (segment) ->
    # quick approximation: find distance to many points on segment and take the minimum
    Math.min.apply(null, @getDistanceToPoint(segment.findPoint(d)) for d in [0...Math.ceil(segment.getLength())])

  clone: ->
    new ArcSegment(@center.clone(), @radius, @start, @end, @direction)

window.gearsketch.model.ArcSegment = ArcSegment

# ---------------------------
# ------- LineSegment -------
# ---------------------------
class LineSegment
  constructor: (@start, @end) ->

  getLength: ->
    @start.distance(@end)

  findPoint: (distanceToGo) ->
    Util.getPointOnLineSegment(@start, @end, distanceToGo)

  getDistanceToPoint: (point) ->
    Util.getPointLineSegmentDistance(point, @start, @end)

  getDistanceToSegment: (segment) ->
    if segment instanceof LineSegment
      if Util.findLineSegmentIntersection([@start, @end], [segment.start, segment.end])
        0
      else
        Math.min(
          Util.getPointLineSegmentDistance(@start, segment.start, segment.end)
        , Util.getPointLineSegmentDistance(@end, segment.start, segment.end)
        , Util.getPointLineSegmentDistance(segment.start, @start, @end)
        , Util.getPointLineSegmentDistance(segment.end, @start, @end))
    else # segment is ArcSegment
      segment.getDistanceToSegment(this)

  clone: ->
    new LineSegment(@start.clone(), @end.clone())

window.gearsketch.model.LineSegment = LineSegment

# ---------------------------
# ---------- Chain ----------
# ---------------------------
# TODO:
# connecting chain to gear with outerRadius, means that two gears
# connected by a chain will not necessarily have the same tooth speed
# use pitch radius instead?
class Chain
  # imports
  ArcSegment = window.gearsketch.model.ArcSegment
  LineSegment = window.gearsketch.model.LineSegment

  @WIDTH: 6

  nextChainId = 0

  points: []
  segments: []

  constructor: (stroke, id) ->
    if id?
      @id = id
    else
      @id = nextChainId
      nextChainId++
    @points = Util.clone(stroke)

  getLength: ->
    length = 0
    for segment in @segments
      length += segment.getLength()
    length

  findPointOnChain: (distance) ->
    length = @getLength()
    distanceToGo = Util.mod(distance, length)
    for segment in @segments
      segmentLength = segment.getLength()
      if distanceToGo < segmentLength
        return segment.findPoint(distanceToGo)
      else
        distanceToGo -= segmentLength
    null

  findPointsOnChain: (numberOfPoints) ->
    delta = @getLength() / numberOfPoints
    @findPointOnChain(p * delta) for p in [0...numberOfPoints]

  getDistanceToPoint: (point) ->
    Math.min.apply(null, (segment.getDistanceToPoint(point) for segment in @segments))

  getDistanceToLineSegment: (lineSegment) ->
    Math.min.apply(null, (segment.getDistanceToSegment(lineSegment) for segment in @segments))

#  removeEmptyTriangles: (board) ->
#    gears = board.getGearList()
#    numberOfNoOps = 0
#    i = 0
#    while numberOfNoOps < (numberOfPoints = @points.length)
#      i = i % numberOfPoints
#      j = (i + 1) % numberOfPoints
#      k = (i + 2) % numberOfPoints
#      triangle = [@points[i], @points[j], @points[k]]
#      if gears.some((gear) -> Util.doesGearIntersectTriangle(gear, triangle))
#        numberOfNoOps++
#        i++
#      else
#        @points.splice(j, 1)
#        numberOfNoOps = 0
#
  # reorder points list if first point is inside a gear
#  ensureFreeFirstPoint: (board) ->
#    freePointIndex = 0
#    numberOfPoints = @points.length
#    for i in [0...numberOfPoints]
#      if board.getGearAt(@points[i]) is null
#        freePointIndex = i
#        break
#    if freePointIndex isnt 0 and i isnt numberOfPoints
#      newFirstPoints = @points.splice(freePointIndex, numberOfPoints - freePointIndex)
#      @points = newFirstPoints.concat(@points)

  findChainTangentSide: (gear) ->
    if (@direction is Util.Direction.CLOCKWISE) is (gear.id in @innerGearIds)
      Util.Side.LEFT
    else
      Util.Side.RIGHT

  findReverseChainTangentSide: (gear) ->
    if @findChainTangentSide(gear) is Util.Side.LEFT
      Util.Side.RIGHT
    else
      Util.Side.LEFT

  findFirstSupportingGearOnPath: (path, gears) ->
    stepSize = 10
    pathLength = Util.getLength(path)
    supportingGear = null
    a = path[0]
    d = 0
    while d < pathLength and !supportingGear?
      #console.log("while1")
      d += stepSize
      b = Util.findPointOnPath(path, d)
      supportingGear = Util.findNearestIntersectingGear(gears, a, b)
    [supportingGear, d]

  findSupportingGearsOnPath: (gears, firstSupportingGear, path, startDistance = 0, isClosed = true) ->
    stepSize = 10
    pathLength = Util.getLength(path, isClosed)
    supportingGear = firstSupportingGear
    supportingGears = []
    a = firstSupportingGear.location
    d = startDistance
    while d < pathLength
      #console.log("while2")
      d += stepSize
      b = Util.findPointOnPath(path, d)
      tangentSide = @findReverseChainTangentSide(supportingGear)
      tangentPoint = Util.findGearTangentPoints(b, supportingGear)[tangentSide]
      a = tangentPoint if tangentPoint? # in case point b is in gear
      nextSupportingGear = Util.findNearestIntersectingGear(gears, a, b, Util.makeSet(supportingGear.id))
      if nextSupportingGear?
        supportingGear = nextSupportingGear
        supportingGears.push(supportingGear)
    supportingGears

  removeRepeatedGears: (gearsList) ->
    numberOfNoops = 0
    i = 0
    while numberOfNoops < (numberOfGears = gearsList.length)
      #console.log("while3")
      j = (i + 1) % numberOfGears
      g1 = gearsList[i]
      g2 = gearsList[j]
      if g1 is g2
        gearsList.splice(j, 1)
        numberOfNoops = 0
      else
        numberOfNoops++
        i = (i + 1) % numberOfGears
    gearsList

  containsSuccessiveOverlappingGears: (gearsList) ->
    numberOfGears = gearsList.length
    for i in [0...numberOfGears]
      j = (i + 1) % numberOfGears
      g1 = gearsList[i]
      g2 = gearsList[j]
      if g1.location.distance(g2.location) < (g1.outerRadius + g2.outerRadius)
        return true
    false

  findSupportingGearIds: (gears) ->
    [firstSupportingGear, startDistance] = @findFirstSupportingGearOnPath(@points, gears)
    supportingGears = [firstSupportingGear]
    nextSupportingGears = @findSupportingGearsOnPath(gears, firstSupportingGear, @points, startDistance)
    supportingGears = supportingGears.concat(nextSupportingGears)
    # if first point is not on first supporting gear, trace path connecting them
    tangentSide = @findChainTangentSide(firstSupportingGear)
    tangentPoint = Util.findGearTangentPoints(@points[0], firstSupportingGear)[tangentSide]
    if tangentPoint?
      finalSegment = [@points[0], tangentPoint]
      lastSupportingGear = supportingGears[supportingGears.length-1]
      nextSupportingGears = @findSupportingGearsOnPath(gears, lastSupportingGear, finalSegment, 0, false)
      supportingGears = supportingGears.concat(nextSupportingGears)
    (gear.id for gear in @removeRepeatedGears(supportingGears))

  findIgnoredGearIds: (board) ->
    # find minimal distance of each level of gears in each group to the chain
    gears = board.getGears()
    minDistances = {}
    for own id, gear of gears
      group = board.getGroup(gear)
      level = board.getLevel(gear)
      d = Util.getPointPathDistance(gear.location, @points) - gear.pitchRadius
      if !minDistances[group]?[level]? or d < minDistances[group][level]
        minDistances[group] ?= {}
        minDistances[group][level] = d

    # find the one level of each group of gears that is not ignored
    # this is currently the closest, nonoverlapping level, a heuristic that can be further improved
    acknowledgedLevels = {}
    for own group, levels of minDistances
      for own level, distance of levels
        currentLevel = acknowledgedLevels[group]
        if !currentLevel?
          acknowledgedLevels[group] = parseInt(level, 10)
        else if distance > 0
          currentDistance = minDistances[group][currentLevel]
          if currentDistance < 0 or distance < currentDistance
            acknowledgedLevels[group] = parseInt(level, 10)

    # ignore all gears not on an acknowledged level
    ignoredGearIds = {}
    for own id, gear of gears
      if acknowledgedLevels[board.getGroup(gear)] isnt board.getLevel(gear)
        ignoredGearIds[id] = true
    ignoredGearIds

  # convert a list of segments to a polygon
  # used for finding gear centers inside a tightened chain
  toPolygon: (segments)->
    polygon = []
    for segment in segments
      if segment instanceof LineSegment
        polygon.push(segment.start)
      else # ArcSegment
        polygon.push(segment.findPoint(0))
        polygon.push(segment.findPoint(0.5 * segment.getLength()))
    polygon

  updateChain: (board, gears = board.getGearsWithIds(@supportingGearIds)) ->
    if gears.length < 2
      return false

    if @containsSuccessiveOverlappingGears(gears)
      return false

    acknowledgedGears = board.getAcknowledgedGears(@ignoredGearIds)

    # first: update gear sequence
    # TODO: update ignored gears, should include overlapping gears in different groups (needed?)
    i = 0
    while i < (numberOfGears = gears.length)
      #console.log("while4")
      j = (i + 1) % numberOfGears
      k = (i + 2) % numberOfGears
      g1 = gears[i]
      g2 = gears[j]
      g3 = gears[k]

      line1 = Util.findTangentLine(g1, g2, @innerGearIds, @direction)
      line2 = Util.findTangentLine(g2, g3, @innerGearIds, @direction)
      intersection = Util.findLineSegmentIntersection(line1, line2)
      if intersection? # g2 cannot support chain
        tangentSideG1 = @findReverseChainTangentSide(g1)
        tangentPointG1 = Util.findGearTangentPoints(intersection, g1)[tangentSideG1]
        tangentSideG3 = @findChainTangentSide(g3)
        tangentPointG3 = Util.findGearTangentPoints(intersection, g3)[tangentSideG3]
        path = [tangentPointG1, intersection, tangentPointG3]
        replacementGears = @findSupportingGearsOnPath(acknowledgedGears, g1, path, 0, false)
        if g2 in replacementGears # rare bug due to floating point errors
          console.log("it happened!")
          return false
        gears.splice.apply(gears, [j, 1].concat(replacementGears))
        @removeRepeatedGears(gears)
        return @updateChain(board, gears) # start over
      gear = Util.findNearestIntersectingGear(acknowledgedGears, line1[0], line1[1], Util.makeSet(g1.id, g2.id))
      if gear?
        gears.splice(j, 0, gear)
        if @containsSuccessiveOverlappingGears(gears)
          return false
      i++

    # second: update points & segments
    updatedPoints = []
    for i in [0...numberOfGears]
      j = (i + 1) % numberOfGears
      g1 = gears[i]
      g2 = gears[j]
      tangentLine = Util.findTangentLine(g1, g2, @innerGearIds, @direction)
      updatedPoints.push(tangentLine[0], tangentLine[1])
    updatedSegments = []
    for i in [0...numberOfGears]
      p0 = updatedPoints[2 * i]
      p1 = updatedPoints[2 * i + 1]
      p2 = updatedPoints[2 * ((i + 1) % numberOfGears)]
      gear = gears[(i + 1) % numberOfGears]
      lineSegment = new LineSegment(p0, p1)
      arcStart = Math.atan2(p1.y - gear.location.y, p1.x - gear.location.x)
      arcEnd = Math.atan2(p2.y - gear.location.y, p2.x - gear.location.x)
      direction = if (@direction is Util.Direction.CLOCKWISE) is (gear.id in @innerGearIds)
        Util.Direction.CLOCKWISE
      else
        Util.Direction.COUNTER_CLOCKWISE
      arcSegment = new ArcSegment(gear.location, gear.pitchRadius, arcStart, arcEnd, direction)
      updatedSegments.push(lineSegment, arcSegment)

    # third: check if chain doesn't touch itself
    numberOfSegments = updatedSegments.length
    for i in [0...numberOfSegments - 2]
      for j in [(i + 2)...numberOfSegments]
        if i isnt 0 or j isnt numberOfSegments - 1 # don't compare first and last segments
          s1 = updatedSegments[i]
          s2 = updatedSegments[j]
          if s1.getDistanceToSegment(s2) < (Chain.WIDTH / 2 + Util.EPSILON) # TODO: don't divide by two?
            # make sure segments are not connected by a very small ArcSegment
            if (i + 2) is j
              middleSegment = updatedSegments[i + 1]
              if (middleSegment instanceof ArcSegment) and (middleSegment.getLength() < Chain.WIDTH)
                continue
            if ((j + 2) % numberOfSegments) is i # note: not else if, both can be true in chain with 4 segments
              middleSegment = updatedSegments[(j + 1) % numberOfSegments]
              if (middleSegment instanceof ArcSegment) and (middleSegment.getLength() < Chain.WIDTH)
                continue
            console.log("chain crosses itself")
            return false

    # fourth: check if innergears hasn't changed
    chainPolygon = @toPolygon(updatedSegments)
    updatedInnerGearIds =
      (gear.id for id, gear of acknowledgedGears when Util.isPointInsidePolygon(gear.location, chainPolygon))
    if !Util.areArraysEqual(@innerGearIds, updatedInnerGearIds)
      console.log("innerGears changed")
      return false

    @points = updatedPoints
    @segments = updatedSegments
    @supportingGearIds = (gear.id for gear in gears)
    true

  tightenChain: (board) ->
    #@removeEmptyTriangles(board)
    #@ensureFreeFirstPoint(board)
    @ignoredGearIds = @findIgnoredGearIds(board)
    acknowledgedGears = board.getAcknowledgedGears(@ignoredGearIds)
    @innerGearIds = (gear.id for gear in Util.findGearsInsidePolygon(@points, acknowledgedGears))
    if @innerGearIds.length < 2
      return false
    @direction = Util.findDirection(@points)
    @supportingGearIds = @findSupportingGearIds(acknowledgedGears)
    @updateChain(board)

  clone: ->
    copy = new Chain(@points, @id)
    copy.segments = Util.clone(@segments)
    copy.ignoredGearIds = Util.clone(@ignoredGearIds)
    copy.innerGearIds = Util.clone(@innerGearIds)
    copy.direction = @direction
    copy.supportingGearIds = Util.clone(@supportingGearIds)
    copy

window.gearsketch.model.Chain = Chain


# ---------------------------
# ---------- Board ----------
# ---------------------------
# TODO:
# - make it possible for multiple connections to exist between gears in some cases (e.g. two chains on different sides)

class Board
  # -- imported constants --
  MODULE = Util.MODULE
  AXIS_RADIUS = Util.AXIS_RADIUS
  MIN_STACKED_GEARS_TEETH_DIFFERENCE = Util.MIN_STACKED_GEARS_TEETH_DIFFERENCE
  SNAPPING_DISTANCE = Util.SNAPPING_DISTANCE
  EPSILON = Util.EPSILON

  ConnectionType =
    ANY: "any"
    MESHING: "meshing"
    AXIS: "axis"

  gears: {}
  connections: {}
  groups: {}
  levels: {}
  momenta: {}
  chains: {}

  # TODO: should probably add board.clone() method
  saveBoard: ->
    gears: Util.clone(@gears)
    connections: Util.clone(@connections)
    groups: Util.clone(@groups)
    levels: Util.clone(@levels)
    momenta: Util.clone(@momenta)
    chains: Util.clone(@chains)

  restoreBoard: (board) ->
    for own id, gear of @gears
      gear.location = board.gears[id].location
      gear.rotation = board.gears[id].rotation
    @connections = board.connections
    @groups = board.groups
    @levels = board.levels
    @chains = board.chains

  restoreBoardAfterDemo: (board) ->
    @gears = board.gears
    @connections = board.connections
    @groups = board.groups
    @levels = board.levels
    @momenta = board.momenta
    @chains = board.chains

  clearBoard: ->
    @gears = {}
    @connections = {}
    @groups = {}
    @levels = {}
    @momenta = {}
    @chains = {}

  getNextGroup: ->
    nextGroup = 0
    for own gear, group of @groups
      nextGroup = Math.max(nextGroup, group + 1)
    nextGroup

  getGears: ->
    @gears

  getGearList: ->
    (gear for own id, gear of @gears)

  getAcknowledgedGears: (ignoredGearIds) ->
    acknowledgedGears = {}
    for id, gear of @gears
      if !(id of ignoredGearIds)
        acknowledgedGears[id] = gear
    acknowledgedGears

  getLevelScore: (gear) ->
    1000 * @groups[gear.id] + @levels[gear.id]

  getGearsSortedByGroupsAndLevel: (gears = @getGearList()) ->
    gearsWithLevelScore = []
    for gear in gears
      score = @getLevelScore(gear)
      gearsWithLevelScore.push([gear, score])
    gearsWithLevelScore.sort((g1, g2) -> g1[1] - g2[1])
    (gearWithLevelScore[0] for gearWithLevelScore in gearsWithLevelScore)

  removeConnection: (gear1, gear2) ->
    delete @connections[gear1.id][gear2.id]
    delete @connections[gear2.id][gear1.id]

  removeAllConnections: (gear) ->
    for own connectedGearId, connectionType of @connections[gear.id]
      connectedGear = @gears[connectedGearId]
      @removeConnection(gear, connectedGear)
    @updateGroupsAndLevels()

  findNearestAxis: (gear) ->
    nearestAxis = null
    shortestDistance = Number.MAX_VALUE
    for own id, candidate of @gears
      if candidate isnt gear
        distance = gear.location.distance(candidate.location)
        if !nearestAxis or
        distance < (shortestDistance - EPSILON) or
        (distance < (shortestDistance + EPSILON) and
        candidate.numberOfTeeth < nearestAxis.numberOfTeeth)
          nearestAxis = candidate
          shortestDistance = distance
    nearestAxis

  updateGroupsAndLevelsFrom: (gearId, group, level, updatedGroups, updatedLevels) ->
    updatedGroups[gearId] = group
    updatedLevels[gearId] = level
    connections = @connections[gearId]
    for own neighborId, connectionType of connections
      if !(neighborId of updatedGroups)
        if connectionType is ConnectionType.MESHING
          @updateGroupsAndLevelsFrom(neighborId, group, level, updatedGroups, updatedLevels)
        else
          gear = @gears[gearId]
          neighbor = @gears[neighborId]
          if gear.numberOfTeeth > neighbor.numberOfTeeth
            @updateGroupsAndLevelsFrom(neighborId, group, level + 1, updatedGroups, updatedLevels)
          else
            @updateGroupsAndLevelsFrom(neighborId, group, level - 1, updatedGroups, updatedLevels)

  updateGroupsAndLevels: ->
    updatedGroups = {}
    updatedLevels = {}
    group = 0
    for own id, gear of @gears
      if !(id of updatedGroups)
        @updateGroupsAndLevelsFrom(id, group, 0, updatedGroups, updatedLevels)
        group++
    @groups = updatedGroups
    @levels = updatedLevels

  addConnection: (gear1, gear2, connectionType) ->
    @connections[gear1.id][gear2.id] = connectionType
    @connections[gear2.id][gear1.id] = connectionType
    @updateGroupsAndLevels()

  findMeshingNeighbors: (gear) ->
    meshingNeighbors = []
    for own candidateId, candidate of @gears
      if candidate isnt gear and Util.getEdgeDistance(gear, candidate) < EPSILON
        meshingNeighbors.push(candidate)
    meshingNeighbors

  alignGearTeeth: (rotatingGear, meshingGear) ->
    # shorter names for readability
    p1 = rotatingGear.location
    r1 = rotatingGear.rotation
    p2 = meshingGear.location
    r2 = meshingGear.rotation

    # get angles of meshing point and phases at that point
    angle1 = Math.atan2(p2.y - p1.y, p2.x - p1.x)
    angle2 = angle1 + Math.PI
    shift1 = Util.mod(angle1 - r1, 2 * Math.PI)
    shift2 = Util.mod(angle2 - r2, 2 * Math.PI)
    toothAngle1 = (2 * Math.PI ) / rotatingGear.numberOfTeeth
    toothAngle2 = (2 * Math.PI) / meshingGear.numberOfTeeth
    phase1 = (shift1 % toothAngle1) / toothAngle1
    phase2 = (shift2 % toothAngle2) / toothAngle2

    # align teeth
    phaseSum = (phase1 + phase2) % 1
    rotatingGear.rotation = r1 + (phaseSum - 0.25) * toothAngle1

  rotateGearsFrom: (gear, angle, rotatedGearIds) ->
    if !(gear.id of rotatedGearIds)
      gear.rotation = Util.mod(gear.rotation + angle, 2 * Math.PI)
      rotatedGearIds[gear.id] = true

    connections = @connections[gear.id]
    for own connectedGearId, connectionType of connections
      connectedGear = @gears[connectedGearId]
      if !(connectedGearId of rotatedGearIds)
        if connectionType is ConnectionType.MESHING
          @rotateGearsFrom(connectedGear, -angle * gear.numberOfTeeth / connectedGear.numberOfTeeth, rotatedGearIds)
        else
          @rotateGearsFrom(connectedGear, angle, rotatedGearIds)

  alignMeshingGears: (gear) ->
    rotatedGearIds = {}
    rotatedGearIds[gear.id] = true
    neighbors = @findMeshingNeighbors(gear)
    for neighbor in neighbors
      @addConnection(gear, neighbor, ConnectionType.MESHING)
      r = neighbor.rotation
      @alignGearTeeth(neighbor, gear)
      angle = neighbor.rotation - r
      rotatedGearIds[neighbor.id] = true
      @rotateGearsFrom(neighbor, angle, rotatedGearIds)


  connectToAxis: (upperGear, lowerGear) ->
    @addConnection(upperGear, lowerGear, ConnectionType.AXIS)
    upperGear.location = lowerGear.location.clone()
    upperGear.rotation = lowerGear.rotation
    @alignMeshingGears(upperGear)

  findNearestNeighbor: (gear, gearIdsToIgnore = {}) ->
    nearestNeighbor = null
    shortestEdgeDistance = Number.MAX_VALUE
    for own neighborId, neighbor of @gears
      if neighbor isnt gear and !(neighborId of gearIdsToIgnore)
        edgeDistance = Util.getEdgeDistance(gear, neighbor)
        if edgeDistance < shortestEdgeDistance
          nearestNeighbor = neighbor
          shortestEdgeDistance = edgeDistance
    nearestNeighbor

  connectToOneMeshingGear: (gear, meshingGear) ->
    delta = gear.location.minus(meshingGear.location)
    angle = Math.atan2(delta.y, delta.x)
    gear.location = meshingGear.location.plus(Point.polar(angle, gear.pitchRadius + meshingGear.pitchRadius))
    @alignGearTeeth(gear, meshingGear)
    @addConnection(gear, meshingGear, ConnectionType.MESHING)

  connectToTwoMeshingGears: (gear, meshingGear1, meshingGear2) ->
    # Finding the correct location is finding the intersection of two circles
    # Based on http://paulbourke.net/geometry/circlesphere/
    p0 = meshingGear1.location
    p1 = meshingGear2.location
    r0 = meshingGear1.pitchRadius + gear.pitchRadius
    r1 = meshingGear2.pitchRadius + gear.pitchRadius

    d = p0.distance(p1)

    # check whether meshing gears are close enough to each other and not
    # on top of each other; connect gear to closest meshing gear otherwise
    if r0 + r1 < d or p0.distance(p1) < EPSILON
      if Util.getEdgeDistance(gear, meshingGear1) < Util.getEdgeDistance(gear, meshingGear2)
        @connectToOneMeshingGear(gear, meshingGear1)
        return
      else
        @connectToOneMeshingGear(gear, meshingGear2)
        return

    # connect gear to both gears
    a = (r0 * r0 - r1 * r1 + d * d) / (2 * d)
    h = Math.sqrt(r0 * r0 - a * a)

    p2 = p0.plus(p1.minus(p0).times(a / d))
    p3x1 = p2.x + h * (p1.y - p0.y) / d
    p3y1 = p2.y - h * (p1.x - p0.x) / d
    p3x2 = p2.x - h * (p1.y - p0.y) / d
    p3y2 = p2.y + h * (p1.x - p0.x) / d
    p3_1 = new Point(p3x1, p3y1)
    p3_2 = new Point(p3x2, p3y2)

    if gear.location.distance(p3_1) < gear.location.distance(p3_2)
      gear.location = p3_1
    else
      gear.location = p3_2

    # recursively align meshing gears
    # may include other gears besides meshingGear1 and meshingGear2
    @alignMeshingGears(gear)

  isGearInGroupWithCycle: (gearId, sourceId, visitedGearIds) ->
    if gearId of visitedGearIds
      true
    else
      visitedGearIds[gearId] = true
      connections = @connections[gearId]
      for own connectedGearId, connectedGear of connections
        if connectedGearId isnt sourceId
          if @isGearInGroupWithCycle(connectedGearId, gearId, visitedGearIds)
            return true
      false

  doesBoardContainCycle: ->
    visitedGearIds = {}
    for own id, gear of @gears
      if !(id of visitedGearIds)
        if @isGearInGroupWithCycle(id, -1, visitedGearIds)
          return true
    false

  isBoardValid: ->
    for own id1, gear1 of @gears
      group1 = @groups[id1]
      level1 = @levels[id1]
      for own id2, gear2 of @gears
        unless gear1 is gear2
          group2 = @groups[id2]
          level2 = @levels[id2]
          axisDistance = gear1.location.distance(gear2.location)
          maxOuterRadius = Math.max(gear1.outerRadius, gear2.outerRadius)
          combinedOuterRadius = gear1.outerRadius + gear2.outerRadius
          if axisDistance < EPSILON
            if group1 isnt group2
              return false
          else if group1 is group2 and level1 is level2 and !@connections[gear1.id][gear2.id]
            if axisDistance < combinedOuterRadius
              return false
          else if axisDistance < maxOuterRadius + AXIS_RADIUS
            return false
    !@doesBoardContainCycle()

  placeGear: (gear, location) ->
    oldBoard = @saveBoard()
    @removeAllConnections(gear)
    gear.location = location.clone()
    nearestAxis = @findNearestAxis(gear)
    if nearestAxis and
    gear.location.distance(nearestAxis.location) < SNAPPING_DISTANCE and
    nearestAxis.numberOfTeeth - gear.numberOfTeeth > MIN_STACKED_GEARS_TEETH_DIFFERENCE
      # connect gear to axis of larger gear
      @connectToAxis(gear, nearestAxis)
    else
      neighbor1 = @findNearestNeighbor(gear)
      if neighbor1 and Util.getEdgeDistance(gear, neighbor1) < SNAPPING_DISTANCE
        # make gear mesh with one or two neighbors
        neighbor2 = @findNearestNeighbor(gear, Util.makeSet(neighbor1.id))
        if neighbor2 and Util.getEdgeDistance(gear, neighbor2) < SNAPPING_DISTANCE
          @connectToTwoMeshingGears(gear, neighbor1, neighbor2)
        else
          @connectToOneMeshingGear(gear, neighbor1)

    # check if board is valid
    if @isBoardValid()
      # TODO: refactor
      chainsValid = true
      for own id, chain of @chains
        chainsValid = chainsValid and chain.updateChain(this)
      if chainsValid
        true
      else
        @restoreBoard(oldBoard)
        false
    else
      @restoreBoard(oldBoard)
      false

  addGear: (gear) ->
    oldBoard = @saveBoard()
    @gears[gear.id] = gear
    @connections[gear.id] = {}
    @groups[gear.id] = @getNextGroup()
    @levels[gear.id] = 0
    if !@placeGear(gear, gear.location)
      @removeGear(gear)
      @restoreBoard(oldBoard)
      false
    else
      true

  getMomentum: (gear) ->
    @momenta[gear.id]

  setMomentum: (gear, momentum) ->
    @momenta[gear.id] = momentum

  removeMomentum: (gear) ->
    delete @momenta[gear.id]

  getGroup: (gear) ->
    @groups[gear.id]

  getLevel: (gear) ->
    @levels[gear.id]

  removeGear: (gear) ->
    @removeMomentum(gear)
    @removeAllConnections(gear)
    delete @gears[gear.id]
    delete @connections[gear.id]
    delete @groups[gear.id]
    delete @levels[gear.id]

  getGearAt: (location) ->
    gear = null
    for own id, candidate of @gears
      distance = location.distance(candidate.location)
      if distance < candidate.outerRadius
        if !gear or candidate.numberOfTeeth < gear.numberOfTeeth
          gear = candidate
    gear

  getGearWithId: (id) ->
    @gears[id]

  getGearsWithIds: (ids) ->
    @gears[id] for id in ids

  rotateAllGears: (delta) ->
    for own id, gear of @gears
      momentum = @getMomentum(gear)
      if momentum
        angle = momentum * delta
        @rotateGearsFrom(gear, angle, {})

  addChain: (chain) ->
    if chain.tightenChain(this)
      @chains[chain.id] = chain

  removeChain: (chain) ->
    delete @chains[chain.id]

  getChains: ->
    @chains

window.gearsketch.model.Board = Board