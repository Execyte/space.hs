#version 330 core

uniform mat4 u_model;
uniform mat4 u_projection;

layout (location = 0) in vec2 position;
layout (location = 1) in vec2 uv;
layout (location = 2) in vec4 color;

out vec2 frag_uv;
out vec4 frag_color;

void main()
{
    gl_Position = u_projection * u_model * vec4(position, 0, 1);
    frag_uv = uv;
    frag_color = color;
}