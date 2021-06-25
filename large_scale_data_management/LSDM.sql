ALTER TABLE specs
ADD PRIMARY KEY ("spec id");


ALTER TABLE labels
ADD FOREIGN KEY ("left spec id") REFERENCES specs("spec id");


ALTER TABLE labels
ADD FOREIGN KEY ("right spec id") REFERENCES specs("spec id");