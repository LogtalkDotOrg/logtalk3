
.. _faq_deployment:

Deployment
==========

* :ref:`faq_deployment_create`

.. _faq_deployment_create:

Can I create standalone applications with Logtalk?
--------------------------------------------------

It depends on the Prolog compiler that you use to run Logtalk. Assuming
that your Prolog compiler supports the creation of standalone executables,
your application must include the adapter file for your compiler and the
Logtalk compiler and runtime. The distribution includes embedding scripts
for selected backend Prolog compilers and embedding examples.

For instructions on how to embed Logtalk and Logtalk applications see
the wiki section on `embedding <https://github.com/LogtalkDotOrg/logtalk3/wiki/Embedding-Logtalk>`_.
