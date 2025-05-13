CREATE TABLE users (
    id UUID NOT NULL PRIMARY KEY,
    email VARCHAR(255) NOT NULL UNIQUE,
    password_hash VARCHAR(255) NOT NULL
);
CREATE INDEX users_email_index ON users(email);

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