CREATE TABLE users (
    id UUID NOT NULL PRIMARY KEY,
    email VARCHAR(128) NOT NULL UNIQUE,
    password_hash VARCHAR(255) NOT NULL
);
CREATE INDEX users_email_index ON users(email);

CREATE TABLE boards (
    id UUID NOT NULL PRIMARY KEY,
    board_name VARCHAR(64),
    user_id UUID NOT NULL,
    CONSTRAINT fk_boards_users FOREIGN KEY (user_id) REFERENCES users(id),
    CONSTRAINT unique_board_users_id UNIQUE (board_name, user_id)
);

CREATE OR REPLACE FUNCTION register_user(_id UUID, _email VARCHAR, _password_hash VARCHAR)
RETURNS BOOLEAN as $$
BEGIN
    IF EXISTS (SELECT email FROM users WHERE email = _email)
        THEN 
            RETURN FALSE;
        ELSE
            INSERT INTO users(id, email, password_hash)
            VALUES(_id, _email, _password_hash);
            RETURN TRUE;
    END IF;
END;
$$ LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION add_board(_id UUID, _board_name VARCHAR, _user_id UUID)
RETURNS BOOLEAN as $$
BEGIN
    IF EXISTS (SELECT id FROM boards WHERE board_name = _board_name AND user_id = _user_id)
        THEN 
            RETURN FALSE;
        ELSE
            INSERT INTO boards(id, board_name, user_id)
            VALUES(_id, _board_name, _user_id);
            RETURN TRUE;
    END IF;
END;
$$ LANGUAGE plpgsql;
