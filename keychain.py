#!/usr/bin/env python

import math
from math import pi
import stlwrite

PHI = (1+math.sqrt(5))/2

#constants for step size of torus making
numlayers = 30
numtheta = 30

def vsum(l1, l2):
    '''returns the elementwise sum of l1 and l2'''
    return list(x+y for x,y in zip(l1,l2))

def vdiff(l1, l2):
    '''returns the elementwise difference of l1 and l2'''
    return list(x-y for x,y in zip(l1,l2))

def times(c,l1):
    '''returns the constant multiplication of c with l1'''
    return list(c*x for x in l1)

def dist(l1, l2):
    '''returns the distance between l1 and l2'''
    sumofsquares = 0
    for i in range(len(l1)):
        sumofsquares += (l1[i]-l2[i]) ** 2
    return math.sqrt(sumofsquares)

def dotp(l1, l2):
    '''returns the dot product of l1 and l2'''
    dotprod = 0
    for i in range(len(l1)):
        dotprod += l1[i]*l2[i]
    return dotprod

def crossp(l1, l2):
    '''returns the cross product of l1 and l2'''
    crossprod = []
    crossprod += [l1[1]*l2[2]-l1[2]*l2[1]]
    crossprod += [l1[2]*l2[0]-l1[0]*l2[2]]
    crossprod += [l1[0]*l2[1]-l1[1]*l2[0]]
    return crossprod

def frange(x, maximum, step):
    while x < maximum:
        yield x
        x += step
    yield maximum
 
def generate_torusXY(R, r, x0, y0):
    '''makes a torus parallel to the XY plane (code mostly copied from duck project)'''
    '''basically the same process as the ellipse, but different parametrization'''
    R0=R
    r0=r

    faces = []
    pointset = []
    phistep = 2*math.pi/(numlayers)
    thetastep = 2*math.pi/(numtheta)
    for phi in frange(0, 2*pi, phistep):
        layer = []
        for theta in frange(0, 2*math.pi, thetastep):
            r=r0
            layer += [[x0+R0*math.cos(theta)+r*math.cos(theta)*math.cos(phi), y0+R0*math.sin(theta)+r*math.sin(theta)*math.cos(phi), 0+r*math.sin(phi)]]
        pointset += [layer]
    #pointset is a numlayers-list of layers. Layers are a (numtheta+1)-list of points. Points are a 3-list of coordinates
    
    for i in range(numlayers):
        if(i != numlayers-1):
            for j in range(numtheta):
                if(j == numtheta - 1):
                    faces += [[pointset[i][j], pointset[i][0], pointset[i+1][0], pointset[i+1][j]]]
                    # faces += [[pointset[i][j], pointset[i][0], pointset[i+1][0]]]
                    # faces += [[pointset[i][j], pointset[i+1][0], pointset[i+1][j]]]
                else:
                    faces += [[pointset[i][j], pointset[i][j+1], pointset[i+1][j+1], pointset[i+1][j]]]
                    # faces += [[pointset[i][j], pointset[i][j+1], pointset[i+1][j+1]]]
                    # faces += [[pointset[i][j], pointset[i+1][j+1], pointset[i+1][j]]]
        else:
            for j in range(numtheta):
                if(j == numtheta - 1):
                    faces += [[pointset[i][j], pointset[i][0], pointset[0][0], pointset[0][j]]]
                    # faces += [[pointset[i][j], pointset[i][0], pointset[0][0]]]
                    # faces += [[pointset[i][j], pointset[0][0], pointset[0][j]]]
                else:
                    faces += [[pointset[i][j], pointset[i][j+1], pointset[0][j+1], pointset[0][j]]]
                    # faces += [[pointset[i][j], pointset[i][j+1], pointset[0][j+1]]]
                    # faces += [[pointset[i][j], pointset[0][j+1], pointset[0][j]]]

    #faces is a list of vertexsets. Vertexsets are a 3,4-list of points. Points are a 3-list of coordinates
    return faces

def generate_torusYZ(R, r, x0, y0):
    '''makes a torus parallel to the YZ plane (code mostly copied from duck project)'''
    R0=R
    r0=r

    faces = []
    pointset = []
    phistep = 2*math.pi/(numlayers)
    thetastep = 2*math.pi/(numtheta)
    for phi in frange(0, 2*pi, phistep):
        layer = []
        for theta in frange(0, 2*math.pi, thetastep):
            r=r0
            layer += [[x0+r*math.sin(phi), y0+R0*math.cos(theta)+r*math.cos(theta)*math.cos(phi), 0+R0*math.sin(theta)+r*math.sin(theta)*math.cos(phi)]]
        pointset += [layer]
    #pointset is a numlayers-list of layers. Layers are a (numtheta+1)-list of points. Points are a 3-list of coordinates
    
    for i in range(numlayers):
        if(i != numlayers-1):
            for j in range(numtheta):
                if(j == numtheta - 1):
                    faces += [[pointset[i][j], pointset[i][0], pointset[i+1][0], pointset[i+1][j]]]
                    # faces += [[pointset[i][j], pointset[i][0], pointset[i+1][0]]]
                    # faces += [[pointset[i][j], pointset[i+1][0], pointset[i+1][j]]]
                else:
                    faces += [[pointset[i][j], pointset[i][j+1], pointset[i+1][j+1], pointset[i+1][j]]]
                    # faces += [[pointset[i][j], pointset[i][j+1], pointset[i+1][j+1]]]
                    # faces += [[pointset[i][j], pointset[i+1][j+1], pointset[i+1][j]]]
        else:
            for j in range(numtheta):
                if(j == numtheta - 1):
                    faces += [[pointset[i][j], pointset[i][0], pointset[0][0], pointset[0][j]]]
                    # faces += [[pointset[i][j], pointset[i][0], pointset[0][0]]]
                    # faces += [[pointset[i][j], pointset[0][0], pointset[0][j]]]
                else:
                    faces += [[pointset[i][j], pointset[i][j+1], pointset[0][j+1], pointset[0][j]]]
                    # faces += [[pointset[i][j], pointset[i][j+1], pointset[0][j+1]]]
                    # faces += [[pointset[i][j], pointset[0][j+1], pointset[0][j]]]

    #faces is a list of vertexsets. Vertexsets are a 3,4-list of points. Points are a 3-list of coordinates
    return faces

def generate_torusXZ(R, r, x0, y0):
    '''makes a torus parallel to the XZ plane (code mostly copied from duck project)'''
    R0=R
    r0=r

    faces = []
    pointset = []
    phistep = 2*math.pi/(numlayers)
    thetastep = 2*math.pi/(numtheta)
    for phi in frange(0, 2*pi, phistep):
        layer = []
        for theta in frange(0, 2*math.pi, thetastep):
            r=r0
            layer += [[x0+R0*math.sin(theta)+r*math.sin(theta)*math.cos(phi), y0+r*math.sin(phi), 0+R0*math.cos(theta)+r*math.cos(theta)*math.cos(phi)]]
        pointset += [layer]
    #pointset is a numlayers-list of layers. Layers are a (numtheta+1)-list of points. Points are a 3-list of coordinates
    
    for i in range(numlayers):
        if(i != numlayers-1):
            for j in range(numtheta):
                if(j == numtheta - 1):
                    faces += [[pointset[i][j], pointset[i][0], pointset[i+1][0], pointset[i+1][j]]]
                    # faces += [[pointset[i][j], pointset[i][0], pointset[i+1][0]]]
                    # faces += [[pointset[i][j], pointset[i+1][0], pointset[i+1][j]]]
                else:
                    faces += [[pointset[i][j], pointset[i][j+1], pointset[i+1][j+1], pointset[i+1][j]]]
                    # faces += [[pointset[i][j], pointset[i][j+1], pointset[i+1][j+1]]]
                    # faces += [[pointset[i][j], pointset[i+1][j+1], pointset[i+1][j]]]
        else:
            for j in range(numtheta):
                if(j == numtheta - 1):
                    faces += [[pointset[i][j], pointset[i][0], pointset[0][0], pointset[0][j]]]
                    # faces += [[pointset[i][j], pointset[i][0], pointset[0][0]]]
                    # faces += [[pointset[i][j], pointset[0][0], pointset[0][j]]]
                else:
                    faces += [[pointset[i][j], pointset[i][j+1], pointset[0][j+1], pointset[0][j]]]
                    # faces += [[pointset[i][j], pointset[i][j+1], pointset[0][j+1]]]
                    # faces += [[pointset[i][j], pointset[0][j+1], pointset[0][j]]]

    #faces is a list of vertexsets. Vertexsets are a 3,4-list of points. Points are a 3-list of coordinates
    return faces


def generate_dodecahedron_vertices(R, x0, y0, z0):
    '''output a list of points of a dodecahedron with circumradius Rsqrt(3) and centered on x0,y0,z0'''
    '''Source: https://en.wikipedia.org/wiki/Regular_dodecahedron#Cartesian_coordinates'''
    points=[]
    for i in [-1,1]:
        for j in [-1,1]:
            for k in [-1,1]:
                points += [vsum([x0,y0,z0],times(R,[i,j,k]))]
    for i in [-1,1]:
        for j in [-1,1]:
                points += [vsum([x0,y0,z0],times(R,[0,i*PHI,j/PHI]))]
    for i in [-1,1]:
        for j in [-1,1]:
                points += [vsum([x0,y0,z0],times(R,[i/PHI,0,j*PHI]))]
    for i in [-1,1]:
        for j in [-1,1]:
                points += [vsum([x0,y0,z0],times(R,[i*PHI,j/PHI,0]))]
    return points #[[x,y,z],[x,y,z],...,[x,y,z]]

def generate_dodecahedron(R, x0, y0, z0):
    '''generates the mesh for a dodecahedron skeleton'''
    '''inspired off of https://i.ytimg.com/vi/YxPVGe0PR5g/maxresdefault.jpg'''
    #constants
    alpha = .15 #controls the width of the skeleton
    beta = .85 #controls the thickness of the skeleton

    faces = [] #[[[x,y,z],[x,y,z],[x,y,z],[x,y,z]],[[x,y,z],[x,y,z],[x,y,z],[x,y,z]],...,[[x,y,z],[x,y,z],[x,y,z],[x,y,z]]]
    outer_vertices = generate_dodecahedron_vertices(R, x0, y0, z0) #[[x,y,z],[x,y,z],...,[x,y,z]]
    adj_list = [] #[[v1,v2,v3],[v1,v2,v3],...,[v1,v2,v3]]
    for vi in range(len(outer_vertices)):
        adj_list += [[]]
        for vj in range(len(outer_vertices)): #brute force searches for neighbros (vertices that are 2*R/PHI distance away from each other)
            if(abs(dist(outer_vertices[vi],outer_vertices[vj])-2*R/PHI) < R/100):
                adj_list[vi] += [vj]

        #linear algebra stuff to list the neighbors in CC order (only works when degree of vertices is 3)
        if(dotp(crossp(vdiff(outer_vertices[adj_list[vi][1]],outer_vertices[adj_list[vi][0]]),vdiff(outer_vertices[adj_list[vi][2]],outer_vertices[adj_list[vi][0]])), outer_vertices[vi]) < 0):
            adj_list[vi][1],adj_list[vi][2] = adj_list[vi][2],adj_list[vi][1]
    

    '''now for each vertex, we take it's neighbors and find the points outlining the face in the counterclockwise direction'''
    for vi in range(len(outer_vertices)):
        for j in range(3):
            vj = adj_list[vi][j]
            vk = adj_list[vi][(j+1)%3]

            p1 = outer_vertices[vi]
            p2 = outer_vertices[vj]
            q1 = outer_vertices[vk]
            p3 = vsum(times(alpha,vsum(vdiff(p2,p1),vdiff(q1,p1))),times(1,p1))

            l = adj_list[vj].index(vi)
            vm = adj_list[vj][(l+2)%3]
            q2 = outer_vertices[vm]

            p4 = vsum(times(alpha,vsum(vdiff(q2,p2),vdiff(p1,p2))),times(1,p2))

            faces += [[p1,p2,p4,p3]]

            p5 = times(beta,p1)
            p6 = times(beta,p2)

            faces += [[p3,p4,p6,p5]]
            print(dist(p1,p3),dist(p3,p5),"\n")

    return faces

def generate_icosahedron_vertices(R, x0, y0, z0):
    '''output a list of points of a dodecahedron with circumradius Rsqrt(5) and centered on x0,y0,z0'''
    '''Source: https://en.wikipedia.org/wiki/Regular_icosahedron#Cartesian_coordinates'''
    points = []
    for i in [-1,1]:
        for j in [-1,1]:
            points += [vsum([x0,y0,z0],times(R,[i,j*PHI,0]))]
            points += [vsum([x0,y0,z0],times(R,[0,i,j*PHI]))]
            points += [vsum([x0,y0,z0],times(R,[j*PHI,0,i]))]
    return points #[[x,y,z],[x,y,z],...,[x,y,z]]

def generate_icosahedron(R, x0, y0, z0):
    '''generates the mesh for a solid icosahedron'''

    faces = [] #[[[x,y,z],[x,y,z],[x,y,z],[x,y,z]],[[x,y,z],[x,y,z],[x,y,z],[x,y,z]],...,[[x,y,z],[x,y,z],[x,y,z],[x,y,z]]]
    outer_vertices = generate_icosahedron_vertices(R, x0, y0, z0) #[[x,y,z],[x,y,z],...,[x,y,z]]
    adj_list = [] #[[v1,v2,v3,v4,v5],[v1,v2,v3,v4,v5],...,[v1,v2,v3,v4,v5]]
    for vi in range(len(outer_vertices)):
        adj_list += [[]]
        temp_list = []
        for vj in range(len(outer_vertices)): #brute force searches for neighbros (vertices that are 2*R/PHI distance away from each other)
            if(abs(dist(outer_vertices[vi],outer_vertices[vj])-R*2) < R/100):
                temp_list += [vj]
        adj_list[vi] += [temp_list[0]]

        #linear algebra stuff to order the neighbors in CC order
        for j in range(1,5):
            vj = adj_list[vi][j-1]
            for vk in temp_list:
                if(abs(dist(outer_vertices[vj],outer_vertices[vk])-R*2) < R/100 and dotp(crossp(outer_vertices[vj],outer_vertices[vk]),outer_vertices[vi]) > 0):
                    adj_list[vi] += [vk]

    
    for vi in range(len(outer_vertices)):
        for j in range(5):
            vj = adj_list[vi][j]
            k = adj_list[vj].index(vi)
            vl = adj_list[vj][(k+4)%5]

            p1 = outer_vertices[vi]
            p2 = outer_vertices[vj]
            p3 = outer_vertices[vl]

            faces += [[p1,p2,p3]]

    return faces



def get_features():
    scale=9

    dodecahedronR = 2*scale #constant associated with size of dodecahedron
    icosahedronR = .6*dodecahedronR

    ringRo = .3*scale #constant associated with revolution radius of chain links
    ringRi = .06*scale #constant associated with tube radius of chain links
    bigringRo = .6*scale #constant associated with revolution radius of largest ring
    gap = 3.4 #constant associated with gap of chain links (should be slightly greater than 2)
    n = 3 #constant associated with number of chain links

    body_parts = []
    body_parts += [generate_dodecahedron(dodecahedronR, 0, 0, 0)]
    for i in range(n): #generates 2*n chain links
        body_parts += [generate_torusXY(ringRo, ringRi, PHI*(dodecahedronR+(ringRo-ringRi)/math.sqrt(3)), (2*i)*(2*ringRo-gap*ringRi)+(1/PHI)*(dodecahedronR+(ringRo-ringRi)/math.sqrt(3)))]
        body_parts += [generate_torusYZ(ringRo, ringRi, PHI*(dodecahedronR+(ringRo-ringRi)/math.sqrt(3)), (2*i+1)*(2*ringRo-gap*ringRi)+(1/PHI)*(dodecahedronR+(ringRo-ringRi)/math.sqrt(3)))]
    body_parts += [generate_torusXY(ringRo, ringRi, PHI*(dodecahedronR+(ringRo-ringRi)/math.sqrt(3)), (2*n)*(2*ringRo-gap*ringRi)+(1/PHI)*(dodecahedronR+(ringRo-ringRi)/math.sqrt(3)))]
    body_parts += [generate_torusXZ(bigringRo, ringRi, -(bigringRo)+PHI*(dodecahedronR+(ringRo-ringRi)/math.sqrt(3)), (ringRo-gap*ringRi)+(2*n)*(2*ringRo-gap*ringRi)+(1/PHI)*(dodecahedronR+(ringRo-ringRi)/math.sqrt(3)))]
    body_parts += [generate_icosahedron(icosahedronR, 0, 0, 0)]
    return body_parts

def write_simple_stl():
    parts = get_features()
    # Put all the faces together in one big list.
    faces = []
    for part in parts:
        faces.extend(part)
    # Write the list as an STL file
    filename = "simple_kc.stl"
    with open(filename, 'wb') as fp:
        writer = stlwrite.ASCII_STL_Writer(fp)
        writer.add_faces(faces)
        writer.close()
    print("Wrote " + filename)



write_simple_stl()