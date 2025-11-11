#version 330 core

uniform sampler2D u_texture;

in vec2 frag_uv;
in vec4 frag_color;

out vec4 gl_FragColor;

void main()
{
    gl_FragColor = texture(u_texture, frag_uv) * frag_color;
}