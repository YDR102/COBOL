DROP TABLE IF EXISTS EMPLEADOS;
CREATE TABLE EMPLEADOS
(
    MATRICULA CHAR(4) NOT NULL,
    APELLIDO CHAR(30) NOT NULL,
    NOMBRE CHAR(20) NOT NULL,
    CATEGORIA CHAR(40) NOT NULL,
    DEPARTAMENTO CHAR(40) NOT NULL,
    SECCION CHAR(30) NOT NULL,
    SALARIO DECIMAL(7,3),
    FECHA_INGRESO DATE,
    FECHA_NACIMIENTO DATE,
    CONSTRAINT PK_MATRICULA PRIMARY KEY (MATRICULA)
);

CREATE UNIQUE INDEX IDX_MATRICULA ON EMPLEADOS (MATRICULA);

INSERT INTO EMPLEADOS
VALUES
    ('1011', 'Gorton', 'Hazel', 'Auxiliar Contable', 'Contabilidad',
        'Copiadoras', 2759.800, '1986-03-02', '1964-11-21'),
    ('1012', 'Preston', 'Liza', 'Ing. Mecanico', 'Ingenieria',
        'Copiadoras', 2901.500, '2022-01-01', '1964-12-02'),
    ('1041', 'Tercan', 'Robert', 'Aux. Admin. Unidad', 'I + D',
        'Impresoras', 2804.400, '1940-08-09', '1965-01-25'),
    ('1054', 'Smith', 'Howard', 'Aux. Diseno', 'Diseno',
        'Copiadoras', 2517.600, '1970-08-07', '1967-08-09'),
    ('1067', 'Scote', 'Gail', 'Especialista Diseno', 'Diseno',
        'Fax', 3694.300, '1948-06-08', '1961-09-30'),
    ('1068', 'Mann', 'Alyssa', 'Ing. Mecanico', 'Ingenieria',
        'Impresoras', 4788.300, '1958-03-19', '1961-10-11'),
    ('1075', 'Kane', 'Sheryl', 'Aux. Diseno', 'Diseno',
        'Impresoras', 2323.900, '1967-01-15', '1969-08-28'),
    ('1055', 'Albert', 'Maxine', 'Aux. Admin. Unidad',
        'Mercado', 'Copiadoras', 2323.005, '1964-11-21', '1954-05-09'),
    ('1056', 'Gonzales', 'Joe', 'Director Unidad',
        'Administracion', 'Copiadoras', 2239.005, '1964-11-21', '1964-11-13'),
    ('1076', 'McKormick', 'Brad', 'Ingeniero Jefe',
        'Ingenieria', 'Copiadoras', 2901.500, '2022-01-01', '1940-08-09');