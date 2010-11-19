create table watch_user (
       email text primary key,
       nickname text not null,
       password text not null);

create table watch_object (
       email text,
       object_type text,
       id text,
       password text,
       constraint watch_object_pkey primary key (email, object_type));